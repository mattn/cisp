#define _CRT_SECURE_NO_WARNINGS 1
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <memory.h>
#include <ctype.h>
#ifndef _MSC_VER
# include <inttypes.h>
# include <unistd.h>
# define _printf_(a,b) __attribute__ ((format (printf, a, b)))
#else
# include <io.h>
# define strdup(x) _strdup(x)
# define isatty(f) _isatty(f)
# define fileno(f) _fileno(f)
# define snprintf(b,n,f,...) _snprintf(b,n,f,__VA_ARGS__)
# define _printf_(a,b)
#endif

#include "cisp.h"
#include "parser.h"
#include "util.h"

static void print_node(BUFFER *buf, NODE *node, int mode);
static NODE* do_ident_global(ENV *env, NODE *node);
static NODE* do_progn(ENV *env, NODE *alist);

static void
buf_init(BUFFER *b) {
  b->ptr = NULL;
  b->len = 0;
  b->pos = 0;
}

static void
buf_append(BUFFER *b, const char *s) {
  size_t len = strlen(s);
  if (b->pos + len + 1 > b->len) {
    b->ptr = (char*)realloc(b->ptr, b->len + len + 100);
    *(b->ptr + b->pos) = 0;
    b->len += len + 100;
  }
  while (*s) {
    *(b->ptr + b->pos++) = *s++;
  }
  *(b->ptr + b->pos) = 0;
}

static void
buf_free(BUFFER *b) {
  free(b->ptr);
}

static void
dump_node(NODE *node) {
  BUFFER buf;
  buf_init(&buf);
  print_node(&buf, node, 0);
  puts(buf.ptr);
  buf_free(&buf);
}

static ENV*
global_env(ENV *env) {
  static ENV *global;

  if (global == NULL) {
    while (env->p) env = env->p;
    global = env;
  }
  return global;
}

void
skip_white(SCANNER *s) {
  int c;
  while (!s_eof(s)) {
    c = s_peek(s);
    if (c == -1) break;
    else if (isspace(c)) s_getc(s);
    else if (c == ';') {
      s_getc(s);
      while (!s_eof(s) && s_peek(s) != '\n') s_getc(s);
    } else break;
  }
}

NODE*
new_node() {
  NODE* node = (NODE*)malloc(sizeof(NODE));
  memset(node, 0, sizeof(NODE));
  node->t = NODE_NIL;
  node->r++;
  return node;
}

static NODE*
new_error(const char* msg) {
  NODE *node = new_node();
  node->t = NODE_ERROR;
  node->s = strdup(msg);
  return node;
}

_printf_(1, 0)
static NODE*
new_errorf(const char* fmt, ...) {
  char buf[BUFSIZ];
  va_list list;
  va_start(list, fmt);
  vsnprintf(buf, sizeof(buf), fmt, list);
  va_end(list);
  return new_error(buf);
}

static NODE*
new_errorn(const char* msg, NODE *n) {
  NODE* node;
  BUFFER buf;
  buf_init(&buf);
  buf_append(&buf, msg);
  buf_append(&buf, ": ");
  print_node(&buf, n, 0);
  node = new_error(buf.ptr);
  return node;
}

ENV*
new_env(ENV *p) {
  ENV* env = (ENV*)malloc(sizeof(ENV));
  memset(env, 0, sizeof(ENV));
  env->p = p;
  env->r++;
  return env;
}

int
node_narg(NODE *node) {
  int i = 0;
  if (node_isnull(node)) return 0;
  i++;
  while (!node_isnull(node->cdr)) {
    node = node->cdr;
    i++;
  }
  return i;
}

static void
print_args(BUFFER *buf, NODE *node, int mode) {
  while (node) {
    buf_append(buf, " ");
    print_node(buf, node->car, mode);
    node = node->cdr;
  }
}

static void
print_cell(BUFFER *buf, NODE *node, int mode) {
  buf_append(buf, "(");

  while (node) {
    if (node->car)
      print_node(buf, node->car, mode);
    else
      buf_append(buf, "nil");
    if (node_isnull(node->cdr))
      break;
    if (node->cdr->t != NODE_CELL) {
      buf_append(buf, " . ");
      print_node(buf, node->cdr, mode);
      break;
    }
    buf_append(buf, " ");
    node = node->cdr;
  }

  buf_append(buf, ")");
}

static void
print_str(BUFFER *buf, NODE *node, int mode) {
  char tmp[2];
  const char* p;
  if (mode) {
    buf_append(buf, node->s);
    return;
  }
  p = node->s;
  if (!p) {
    buf_append(buf, "nil");
    return;
  }
  tmp[1] = 0;
  buf_append(buf, "\"");
  while (*p) {
    switch (*p) {
    case '\\': buf_append(buf, "\\\\"); p++; continue; break;
    case '\n': buf_append(buf, "\\n"); p++; continue; break;
    case '\b': buf_append(buf, "\\b"); p++; continue; break;
    case '\f': buf_append(buf, "\\f"); p++; continue; break;
    case '\r': buf_append(buf, "\\r"); p++; continue; break;
    case '\t': buf_append(buf, "\\t"); p++; continue; break;
    }
    tmp[0] = *p;
    buf_append(buf, tmp);
    p++;
  }
  buf_append(buf, "\"");
}

static void
print_float(BUFFER *buf, NODE *node) {
  char tmp[BUFSIZ];
  snprintf(tmp, sizeof(tmp), "%lf", node->d);
  if (node->d == (double)(int)(node->d)) {
    char *p = tmp + strlen(tmp) - 1;
    while (p > tmp && *(p - 1) == '0') *p-- = 0;
  }
  buf_append(buf, tmp);
}

static void
print_node(BUFFER *buf, NODE *node, int mode) {
  char tmp[BUFSIZ];
  if (!node) {
    buf_append(buf, "nil");
    return;
  }
  switch (node->t) {
  case NODE_INT:
    snprintf(tmp, sizeof(tmp)-1, "%ld", node->i);
    buf_append(buf, tmp);
    break;
  case NODE_DOUBLE:
    print_float(buf, node);
    break;
  case NODE_STRING:
    print_str(buf, node, mode);
    break;
  case NODE_IDENT:
    snprintf(tmp, sizeof(tmp)-1, "%s", node->s);
    buf_append(buf, tmp);
    break;
  case NODE_NIL:
    buf_append(buf, "nil");
    break;
  case NODE_T:
    buf_append(buf, "t");
    break;
  case NODE_QUOTE:
    buf_append(buf, "'");
    print_node(buf, node->car, mode);
    break;
  case NODE_BQUOTE:
    buf_append(buf, "`");
    print_node(buf, node->car, mode);
    break;
  case NODE_CELL:
    print_cell(buf, node, mode);
    break;
  case NODE_AREF:
    buf_append(buf, "(aref");
    print_args(buf, node->car, mode);
    buf_append(buf, ")");
    break;
  case NODE_LAMBDA:
    buf_append(buf, "(lambda");
    print_args(buf, node->cdr, mode);
    buf_append(buf, ")");
    break;
  default:
    buf_append(buf, "()");
    break;
  }
}

void
free_node(NODE *node) {
  if (!node) return;
  node->r--;
  if (node->r > 0) return;
  switch (node->t) {
  case NODE_LAMBDA:
  case NODE_QUOTE:
  case NODE_BQUOTE:
  case NODE_CELL:
    if (node->cdr) free_node(node->cdr);
    if (node->car) free_node(node->car);
    break;
  case NODE_STRING:
  case NODE_IDENT:
  case NODE_ERROR:
  case NODE_NIL:
  case NODE_T:
    free((void*)node->s);
    break;
  case NODE_ENV:
    free_env((ENV*) node->p);
    break;
  default:
    break;
  }
  free((void*)node);
}

void
free_env(ENV *env) {
  int i;
  env->r--;
  if (env->r > 0) return;
  for (i = 0; i < env->nv; i++) {
    free_node(env->lv[i]->v);
    free((void*)env->lv[i]->k);
    free((void*)env->lv[i]);
  }
  for (i = 0; i < env->nf; i++) {
    free_node(env->lf[i]->v);
    free((void*)env->lf[i]->k);
    free((void*)env->lf[i]);
  }
  for (i = 0; i < env->nm; i++) {
    free_node(env->lm[i]->v);
    free((void*)env->lm[i]->k);
    free((void*)env->lm[i]);
  }
  free((void*)env->lv);
  free((void*)env->lf);
  free((void*)env->lm);
  free((void*)env);
}

static int
compare_item(const void *a, const void *b) {
  return strcmp((*((ITEM**)a))->k, (*((ITEM**)b))->k);
}

static INLINE void
add_item(ITEM ***ll, int *nl, const char *k, NODE *v) {
  ITEM *ni = (ITEM*)malloc(sizeof(ITEM));
  memset(ni, 0, sizeof(ITEM));
  ni->k = strdup(k);
  ni->v = v;
  *ll = (ITEM**)realloc(*ll, sizeof(ITEM*) * (*nl + 1));
  (*ll)[*nl] = ni;
  (*nl)++;
  qsort(*ll, *nl, sizeof(ITEM*), compare_item);
}

static INLINE ITEM*
find_item(ITEM **ll, int nl, const char *k) {
  int left, right, mid, r;

  left = 0;
  right = nl - 1;
  while (left <= right) {
    mid = (left + right) / 2;
    r = strcmp(ll[mid]->k, k);
    if (r == 0) {
      return ll[mid];
    } else if (r < 0) {
      left = mid + 1;
    } else {
      right = mid - 1;
    }
  }
  return NULL;
}

static NODE*
look_ident(ENV *env, const char *k) {
  ITEM *ni;

  if (!k) return NULL;

  ni = find_item(env->lv, env->nv, k);
  if (ni) {
    ni->v->r++;
    return ni->v;
  }

  if (env->p) return look_ident(env->p, k);

  return NULL;
}

static NODE*
look_func(ENV *env, const char *k) {
  ITEM *ni;

  if (!k) return NULL;

  ni = find_item(env->lf, env->nf, k);
  if (ni) {
    ni->v->r++;
    return ni->v;
  }

  if (env->p) return look_func(env->p, k);

  return NULL;
}

static NODE*
look_macro(ENV *env, const char *k) {
  ENV *global;
  ITEM *ni;

  if (!k) return NULL;

  global = global_env(env);
  ni = find_item(global->lm, global->nm, k);
  if (ni) {
    ni->v->r++;
    return ni->v;
  }

  return NULL;
}

void
add_variable(ENV *env, const char *k, NODE *node) {
  ITEM *ni = find_item(env->lv, env->nv, k);
  if (ni) {
    free_node(ni->v);
    ni->v = node;
    return;
  }
  add_item(&env->lv, &env->nv, k, node);
}

void
add_function(ENV *env, const char *k, NODE *node) {
  ITEM *ni = find_item(env->lf, env->nf, k);
  if (ni) {
    free_node(ni->v);
    ni->v = node;
    return;
  }
  add_item(&env->lf, &env->nf, k, node);
}

void
add_macro(ENV *env, const char *k, NODE *node) {
  ITEM *ni = find_item(env->lm, env->nm, k);
  if (ni) {
    free_node(ni->v);
    ni->v = node;
    return;
  }
  add_item(&env->lm, &env->nm, k, node);
}

static long
int_value(ENV *env, NODE *node, NODE **err) {
  int r = 0;
  if (*err) return 0;
  node = eval_node(env, node);
  switch (node->t) {
  case NODE_ERROR: *err = node; return 0;
  case NODE_NIL: r = 0; break;
  case NODE_T: r = 1; break;
  case NODE_INT: r = node->i; break;
  case NODE_DOUBLE: r = (long)node->d; break;
  case NODE_QUOTE: r = int_value(env, node->car, err); break;
  case NODE_BQUOTE: r = int_value(env, node->car, err); break;
  default: *err = new_error("malformed number"); break;
  }
  free_node(node);
  return r;
}

static double
double_value(ENV *env, NODE *node, NODE **err) {
  double r = 0;
  if (*err) return 0;
  node = eval_node(env, node);
  switch (node->t) {
  case NODE_ERROR: *err = node; return 0;
  case NODE_INT: r = (double)node->i; break;
  case NODE_DOUBLE: r = node->d; break;
  case NODE_QUOTE: r = double_value(env, node->car, err); break;
  case NODE_BQUOTE: r = double_value(env, node->car, err); break;
  default: *err = new_error("malformed number"); break;
  }
  free_node(node);
  return r;
}

static NODE*
do_plus(ENV *env, NODE *alist) {
  NODE *nn, *c, *err = NULL;

  nn = new_node();
  nn->t = NODE_INT;
  nn->i = 0;

  while (!node_isnull(alist)) {
    c = eval_node(env, alist->car);
    if (c->t == NODE_ERROR) {
      free_node(nn);
      return c;
    }
    if (nn->t == NODE_INT) {
      if (c->t == NODE_DOUBLE) {
        nn->d = double_value(env, nn, &err) + double_value(env, c, &err);
        nn->t = c->t;
      } else
        nn->i += int_value(env, c, &err);
    } else {
      nn->d += double_value(env, c, &err);
    }
    free_node(c);
    if (err) {
      free_node(nn);
      return err;
    }
    alist = alist->cdr;
  }
  return nn;
}

static NODE*
do_minus(ENV *env, NODE *alist) {
  NODE *nn, *c, *err = NULL;

  if (node_narg(alist) < 1) return new_errorn("malformed -", alist);

  c = eval_node(env, alist->car);
  if (c->t == NODE_ERROR) return c;
  nn = new_node();
  nn->t = c->t;
  switch (nn->t) {
  case NODE_INT:
    nn->i = c->i;
    break;
  case NODE_DOUBLE:
    nn->d = c->d;
    break;
  default:
    free_node(nn);
    return new_error("malformed number");
  }
  free_node(c);

  alist = alist->cdr;
  if (node_isnull(alist)) {
    if (nn->t == NODE_INT)
      nn->i = -nn->i;
    else
      nn->d = -nn->d;
    return nn;
  }

  while (!node_isnull(alist)) {
    c = eval_node(env, alist->car);
    if (c->t == NODE_ERROR) return c;
    if (nn->t == NODE_INT) {
      if (c->t == NODE_DOUBLE) {
        nn->d = double_value(env, nn, &err) - double_value(env, c, &err);
        nn->t = c->t;
      } else
        nn->i -= int_value(env, c, &err);
    } else {
      nn->d -= double_value(env, c, &err);
    }
    free_node(c);
    if (err) {
      free_node(nn);
      return err;
    }
    alist = alist->cdr;
  }
  return nn;
}

static NODE*
do_mul(ENV *env, NODE *alist) {
  NODE *nn, *c, *err = NULL;

  nn = new_node();
  nn->t = NODE_INT;
  nn->i = 1;

  while (!node_isnull(alist)) {
    c = eval_node(env, alist->car);
    if (c->t == NODE_ERROR) return c;
    if (nn->t == NODE_INT) {
      if (c->t == NODE_DOUBLE) {
        nn->d = double_value(env, nn, &err) * double_value(env, c, &err);
        nn->t = c->t;
      } else
        nn->i *= int_value(env, c, &err);
    } else {
      nn->d *= double_value(env, c, &err);
    }
    free_node(c);
    if (err) {
      free_node(nn);
      return err;
    }
    alist = alist->cdr;
  }
  return nn;
}

static NODE*
do_div(ENV *env, NODE *alist) {
  NODE *nn, *c, *err = NULL;

  if (node_narg(alist) < 1) return new_errorn("malformed /", alist);

  c = eval_node(env, alist->car);
  if (c->t == NODE_ERROR) return c;
  nn = new_node();
  nn->t = c->t;
  switch (nn->t) {
  case NODE_INT:
    nn->i = c->i;
    break;
  case NODE_DOUBLE:
    nn->d = c->d;
    break;
  default:
    free_node(nn);
    return new_error("malformed number");
  }
  free_node(c);

  alist = alist->cdr;
  if (node_isnull(alist)) {
    if (nn->t == NODE_INT)
      nn->i = 1 / nn->i;
    else
      nn->d = 1.0 / nn->d;
    return nn;
  }

  while (!node_isnull(alist)) {
    c = eval_node(env, alist->car);
    if (c->t == NODE_ERROR) return c;
    if (nn->t == NODE_INT) {
      if (c->t == NODE_DOUBLE) {
        nn->d = double_value(env, nn, &err) / double_value(env, c, &err);
        nn->t = c->t;
      } else
        nn->i /= int_value(env, c, &err);
    } else {
      nn->d /= double_value(env, c, &err);
    }
    free_node(c);
    if (err) {
      free_node(nn);
      return err;
    }
    alist = alist->cdr;
  }
  return nn;
}

static NODE*
do_plus1(ENV *env, NODE *alist) {
  NODE *x, *c;

  if (node_narg(alist) != 1) return new_errorn("malformed 1+", alist);

  x = eval_node(env, alist->car);
  if (x->t == NODE_ERROR) return x;

  c = new_node();
  c->t = x->t;
  switch (c->t) {
  case NODE_INT: c->i = x->i + 1; break;
  case NODE_DOUBLE: c->d = x->d + 1.0; break;
  default: free_node(c); c = new_error("malformed number"); break;
  }
  free_node(x);
  return c;
}

static NODE*
do_minus1(ENV *env, NODE *alist) {
  NODE *x, *c;

  if (node_narg(alist) != 1) return new_errorn("malformed 1-", alist);

  x = eval_node(env, alist->car);
  if (x->t == NODE_ERROR) return x;

  c = new_node();
  c->t = x->t;
  switch (c->t) {
  case NODE_INT: c->i = x->i - 1; break;
  case NODE_DOUBLE: c->d = x->d - 1.0; break;
  default: free_node(c); c = new_error("malformed number"); break;
  }
  free_node(x);
  return c;
}

static NODE*
do_and(ENV *env, NODE *alist) {
  NODE *c;

  c = NULL;
  while (!node_isnull(alist)) {
    if (c) free_node(c);
    c = eval_node(env, alist->car);
    if (c->t == NODE_ERROR || c->t == NODE_NIL) break;
    alist = alist->cdr;
  }
  if (c) return c;
  return new_node();
}

static NODE*
do_or(ENV *env, NODE *alist) {
  NODE *c;

  c = NULL;
  while (!node_isnull(alist)) {
    if (c) free_node(c);
    c = eval_node(env, alist->car);
    if (c->t == NODE_ERROR || c->t != NODE_NIL) break;
    alist = alist->cdr;
  }
  if (c) return c;
  return new_node();
}

static NODE*
do_not(ENV *env, NODE *alist) {
  NODE *c, *err = NULL;

  if (node_narg(alist) != 1) return new_errorn("malformed not", alist);

  c = new_node();
  c->t = NODE_INT;
  c->i = !int_value(env, alist->car, &err);
  if (err) {
    free_node(c);
    return err;
  }
  return c;
}

static NODE*
do_evenp(ENV *env, NODE *alist) {
  NODE *c, *err = NULL;

  if (node_narg(alist) != 1) return new_errorn("malformed evenp", alist);

  c = new_node();
  if (int_value(env, alist->car, &err) % 2 == 0) {
    c->t = NODE_T;
  }
  if (err) {
    free_node(c);
    return err;
  }
  return c;
}

static NODE*
do_oddp(ENV *env, NODE *alist) {
  NODE *c, *err = NULL;

  if (node_narg(alist) != 1) return new_errorn("malformed oddp", alist);

  c = new_node();
  if (int_value(env, alist->car, &err) % 2 != 0) {
    c->t = NODE_T;
  }
  if (err) {
    free_node(c);
    return err;
  }
  return c;
}

static NODE*
do_mod(ENV *env, NODE *alist) {
  NODE *c, *err = NULL;

  if (node_narg(alist) != 2) return new_errorn("malformed mod", alist);

  c = new_node();
  c->t = NODE_INT;
  c->i = int_value(env, alist->car, &err) % int_value(env, alist->cdr->car, &err);
  if (err) {
    free_node(c);
    return err;
  }
  return c;
}

static NODE*
do_if(ENV *env, NODE *alist) {
  NODE *c;
  int r = 0, narg;

  narg = node_narg(alist);
  if (narg != 2 && narg != 3) return new_errorn("malformed if", alist);

  c = eval_node(env, alist->car);
  if (c->t == NODE_ERROR) return c;
  switch (c->t) {
  case NODE_NIL:
    r = 0;
    break;
  case NODE_T:
    r = 1;
    break;
  case NODE_INT:
    r = c->i;
    break;
  case NODE_DOUBLE:
    r = (long)c->d;
    break;
  default:
    r = 1;
    break;
  }
  free_node(c);
  if (narg == 2) {
    if (r > 0) return eval_node(env, alist->cdr->car);
    return new_node();
  }
  return eval_node(env, r > 0 ? alist->cdr->car : alist->cdr->cdr->car);
}

static NODE*
do_gt(ENV *env, NODE *alist) {
  NODE *nn, *err = NULL;

  if (node_narg(alist) != 2) return new_errorn("malformed >", alist);

  nn = new_node();
  if (double_value(env, alist->car, &err) > double_value(env, alist->cdr->car, &err)) {
    nn->t = NODE_T;
  }
  if (err) {
    free_node(nn);
    return err;
  }
  return nn;
}

static NODE*
do_ge(ENV *env, NODE *alist) {
  NODE *nn, *err = NULL;

  if (node_narg(alist) != 2) return new_errorn("malformed >=", alist);

  nn = new_node();
  if (double_value(env, alist->car, &err) >= double_value(env, alist->cdr->car, &err)) {
    nn->t = NODE_T;
  }
  if (err) {
    free_node(nn);
    return err;
  }
  return nn;
}

static NODE*
do_lt(ENV *env, NODE *alist) {
  NODE *nn, *err = NULL;

  if (node_narg(alist) != 2) return new_errorn("malformed <", alist);

  nn = new_node();
  if (double_value(env, alist->car, &err) < double_value(env, alist->cdr->car, &err)) {
    nn->t = NODE_T;
  }
  if (err) {
    free_node(nn);
    return err;
  }
  return nn;
}

static NODE*
do_le(ENV *env, NODE *alist) {
  NODE *nn, *err = NULL;

  if (node_narg(alist) != 2) return new_errorn("malformed <=", alist);

  nn = new_node();
  if (double_value(env, alist->car, &err) <= double_value(env, alist->cdr->car, &err)) {
    nn->t = NODE_T;
  }
  if (err) {
    free_node(nn);
    return err;
  }
  return nn;
}

static NODE*
do_eq(ENV *env, NODE *alist) {
  NODE *lhs, *rhs, *nn, *err = NULL;

  if (node_narg(alist) != 2) return new_errorn("malformed =", alist);

  lhs = eval_node(env, alist->car);
  rhs = eval_node(env, alist->cdr->car);
  nn = new_node();
  switch (lhs->t) {
  case NODE_INT:
    if (int_value(env, lhs, &err) == int_value(env, rhs, &err)) {
      nn->t = NODE_T;
    }
    break;
  case NODE_DOUBLE:
    if (double_value(env, lhs, &err) == double_value(env, rhs, &err)) {
      nn->t = NODE_T;
    }
    break;
  case NODE_STRING:
    if (rhs->t == NODE_STRING && !strcmp(lhs->s, rhs->s)) {
      nn->t = NODE_T;
    }
    break;
  default:
    err = new_error("illegal comparing");
    break;
  }
  free_node(lhs);
  free_node(rhs);
  if (err) {
    free_node(nn);
    return err;
  }
  return nn;
}

static NODE*
do_print(ENV *env, NODE *alist) {
  NODE *c;
  BUFFER buf;

  if (node_narg(alist) != 1) return new_errorn("malformed print", alist);

  c = eval_node(env, alist->car);
  if (c->t == NODE_ERROR) return c;
  buf_init(&buf);
  print_node(&buf, c, 0);
  puts(buf.ptr);
  free_node(c);
  c = new_node();
  c->t = NODE_STRING;
  c->s = buf.ptr;
  return c;
}

static NODE*
do_println(ENV *env, NODE *alist) {
  NODE *c;
  BUFFER buf;

  if (node_narg(alist) != 1) return new_errorn("malformed println", alist);

  c = eval_node(env, alist);
  if (c->t == NODE_ERROR) return c;
  buf_init(&buf);
  print_node(&buf, c, 0);
  puts(buf.ptr);
  free_node(c);
  c = new_node();
  c->t = NODE_STRING;
  c->s = buf.ptr;
  return c;
}

static NODE*
do_princ(ENV *env, NODE *alist) {
  NODE *c;
  BUFFER buf;

  if (node_narg(alist) != 1) return new_errorn("malformed printc", alist);

  c = eval_node(env, alist);
  if (c->t == NODE_ERROR) return c;
  buf_init(&buf);
  print_node(&buf, c, 0);
  printf("%s", buf.ptr);
  free_node(c);
  c = new_node();
  c->t = NODE_STRING;
  c->s = buf.ptr;
  return c;
}

static NODE*
do_quote(ENV *env, NODE *alist) {
  UNUSED(env);

  if (node_narg(alist) != 1) return new_errorn("malformed quote", alist);

  alist = alist->car;
  alist->r++;
  return alist;
}

static NODE*
do_let_(ENV *env, NODE *alist, int star) {
  ENV *newenv;
  NODE *x, *c, *n;
  long l;

  if (node_narg(alist) < 1) return new_errorn(star ? "malformed let*" : "malformed let", alist);

  x = alist->car;
  newenv = new_env(env);

  while (!node_isnull(x)) {
    n = x->car;
    if (!n) {
      free_env(newenv);
      return new_errorn(star ? "malformed let*" : "malformed let", alist);
    }
    if (n->t == NODE_IDENT)
      add_variable(newenv, n->s, new_node());
    else if (n->t != NODE_CELL) {
      free_env(newenv);
      return new_errorn(star ? "malformed let*" : "malformed let", alist);
    } else {
      l = node_narg(n);
      if ((l != 1 && l != 2) || !n->car || n->car->t != NODE_IDENT) {
        free_env(newenv);
        return new_errorn(star ? "malformed let*" : "malformed let", alist);
      }
      if (l == 1)
        add_variable(newenv, n->car->s, new_node());
      else {
        c = eval_node(star ? newenv : env, n->cdr->car);
        if (c->t == NODE_ERROR) {
          free_env(newenv);
          return c;
        }
        add_variable(newenv, n->car->s, c);
      }
    }
    x = x->cdr;
  }
  c = do_progn(newenv, alist->cdr);
  free_env(newenv);
  return c;
}

static NODE*
do_list(ENV *env, NODE *alist) {
  int bq;
  NODE *c, *v, *nc, *l = NULL, *rr = NULL;
  UNUSED(env);

  if (alist->t == NODE_BQUOTE) {
    alist = alist->car;
    bq = 1;
  }

  while (!node_isnull(alist)) {
    int expand = 0;
    v = alist->car;
    if (bq && v->t == NODE_IDENT) {
      if (*v->s == '@') {
        expand = 2;
        c = look_ident(env, v->s+1);
      } else {
        expand = 1;
        c = look_ident(env, v->s);
      }
    } else {
      c = eval_node(env, v);
    }
    if (c->t == NODE_ERROR) {
      if (rr) free_node(rr);
      return c;
    }
    if (l == NULL) {
      switch (expand) {
      case 0:
        nc = new_node();
        nc->t = NODE_CELL;
        nc->car = c;
        rr = l = nc;
        break;
      case 1:
        nc = new_node();
        nc->t = NODE_CELL;
        nc->car = c;
        rr = l = nc;
        break;
      case 2:
        rr = l = c->car;
        rr->r++;
        free_node(c);
      }
    } else {
      switch (expand) {
      case 0:
        nc = new_node();
        nc->t = NODE_CELL;
        nc->car = c;
        l->cdr = nc;
        l = l->cdr;
        break;
      case 1:
        nc = new_node();
        nc->t = NODE_CELL;
        nc->car = c;
        l->cdr = nc;
        l = l->cdr;
        break;
      case 2:
        l->cdr = c;
        while (l->cdr)
          l = l->cdr;
        break;
      }
    }
    alist = alist->cdr;
  }
  if (rr) return rr;
  return new_node();
}

static NODE*
do_let(ENV *env, NODE *alist) {
  return do_let_(env, alist, 0);
}

static NODE*
do_let_s(ENV *env, NODE *alist) {
  return do_let_(env, alist, 1);
}

static NODE*
do_flet_labels(ENV *env, NODE *alist, int mode) {
  ENV *newenv;
  NODE *x, *c, *n, *e;

  if (node_narg(alist) < 1) return new_errorn(mode ? "malformed labels" : "malformed flet", alist);

  x = alist->car;
  newenv = new_env(env);

  while (!node_isnull(x)) {
    n = x->car;
    if (node_narg(n) < 2) return new_errorn(mode ? "malformed labels" : "malformed flet", alist);
    if (!n->car || n->car->t != NODE_IDENT || (!node_isnull(n->cdr->car) && n->cdr->car->t != NODE_CELL)) {
      free_env(newenv);
      return new_errorn(mode ? "malformed labels" : "malformed flet", alist);
    }
    e = new_node();
    e->t = NODE_ENV;
    e->p = mode ? newenv : env;
    env->r++;
    n->car->cdr = e;

    add_function(newenv, n->car->s, n);
    n->r++;
    x = x->cdr;
  }
  c = do_progn(newenv, alist->cdr);
  free_env(newenv);
  if (c) return c;
  return new_node();
}

static NODE*
do_flet(ENV *env, NODE *alist) {
  return do_flet_labels(env, alist, 0);
}

static NODE*
do_labels(ENV *env, NODE *alist) {
  return do_flet_labels(env, alist, 1);
}

static NODE*
do_exit(ENV *env, NODE *alist) {
  UNUSED(env);

  if (node_narg(alist)) return new_errorn("malformed exit", alist);

  exit(0);
  return NULL;
}

static NODE*
do_setq(ENV *env, NODE *alist) {
  NODE *x, *c, *last = NULL;

  if (node_narg(alist) < 2) return new_errorn("malformed setq", alist);

  while (alist) {
    if (last) free_node(last);

    x = alist->car;
    if (x->t != NODE_IDENT) {
      return new_errorn("invalid identifier", x);
    }

    c = alist->cdr->car;
    if (!c) break;
    if (c->t == NODE_CELL && c->car && c->car->t == NODE_CELL) {
      c = c->car;
    }
    c = eval_node(env, c);
    if (c->t == NODE_ERROR) return c;
    while (env) {
      ITEM *ni = find_item(env->lv, env->nv, x->s);
      if (!env->p) {
        add_variable(env, x->s, c);
        break;
      }
      if (ni) {
        free_node(ni->v);
        ni->v = c;
        break;
      }
      env = env->p;
    }
    last = c;
    alist = alist->cdr->cdr;
  }
  last->r++;
  if (last->t == NODE_LAMBDA) {
    x = new_node();
    x->t = NODE_QUOTE;
    x->car = last;
    last = x;
  }
  return last;
}

static NODE*
do_setf(ENV *env, NODE *alist) {
  NODE *x, *c, *y, *z, *last = NULL;
  int i, n;

  if (node_narg(alist) != 2) return new_errorn("malformed setf", alist);

  while (alist) {
    if (last) free_node(last);

    x = alist->car;
    if (x->t == NODE_CELL) {
      x = eval_node(env, x);
    }
    switch (x->t) {
    case NODE_AREF:
      c = eval_node(env, alist->cdr->car);
      y = eval_node(env, x->car->car);
      while (x) {
        z = eval_node(env, x->car->cdr);
        if (z->t == NODE_ERROR) return z;
        if (z->t != NODE_INT) return new_errorn("malformed setf", alist);
        n = z->i;
        free_node(z);
        for (i = 0; i < n; i++) {
          y = y->cdr;
        }
        free_node(y->car);
        y->car = c;
        x = x->cdr;
      }
      break;
    case NODE_IDENT:
      c = alist->cdr->car;
      if (!c) break;
      if (c->t == NODE_CELL && c->car && c->car->t == NODE_CELL) {
        c = c->car;
      }
      c = eval_node(env, c);
      if (c->t == NODE_ERROR) return c;
      while (env) {
        ITEM *ni = find_item(env->lv, env->nv, x->s);
        if (!env->p) {
          add_variable(env, x->s, c);
          break;
        }
        if (ni) {
          free_node(ni->v);
          ni->v = c;
          break;
        }
        env = env->p;
      }
      break;
    default:
      return new_errorn("invalid identifier", x);
    }

    last = c;
    alist = alist->cdr->cdr;
  }
  last->r++;
  if (last->t == NODE_LAMBDA) {
    x = new_node();
    x->t = NODE_QUOTE;
    x->car = last;
    last = x;
  }

  return last;
}

static INLINE NODE*
do_ident(ENV *env, NODE *alist) {
  ITEM *ni;

  ni = find_item(env->lv, env->nv, alist->s);
  if (ni) {
    ni->v->r++;
    return ni->v;
  }

  if (env->p) return do_ident(env->p, alist);

  return new_errorf("unknown identity: %s", alist->s);
}

static INLINE NODE*
do_ident_global(ENV *env, NODE *node) {
  ENV *global = global_env(env);
  ITEM *ni;

  ni = find_item(global->lf, global->nf, node->s);
  if (ni) {
    ni->v->r++;
    return ni->v;
  }

  return new_errorf("unknown identify: %s", node->s);
}

#if 0
static NODE*
copy_node(ENV *env, NODE *lhs) {
  ITEM *ni;
  NODE *rhs;

  if (!lhs) return NULL;
  if (lhs->t == NODE_IDENT) {
    ni = find_item(env->lv, env->nv, lhs->s);
    if (ni) {
      ni->v->r++;
      rhs = ni->v;
    } else {
      rhs = new_node();
      rhs->t = lhs->t;
      rhs->s = strdup(lhs->s);
    }
    return rhs;
  }
  rhs = new_node();
  rhs->t = lhs->t;
  switch (lhs->t) {
  case NODE_INT:
    rhs->i = lhs->i;
    break;
  case NODE_DOUBLE:
    rhs->d = lhs->d;
    break;
  case NODE_STRING:
    rhs->s = strdup(lhs->s);
    break;
  case NODE_ERROR:
    rhs->s = strdup(lhs->s);
    break;
  case NODE_LAMBDA:
  case NODE_QUOTE:
  case NODE_CELL:
    rhs->car = copy_node(env, lhs->car);
    rhs->cdr = copy_node(env, lhs->cdr);
    break;
  default:
    break;
  }
  return rhs;
}
#endif

static NODE*
call_node(ENV *env, NODE *node, NODE *alist) {
  ENV *newenv = NULL;
  NODE *x = NULL, *p = NULL, *q = NULL, *c = NULL, *nn = NULL;
  int macro = 0;

  if (node->t == NODE_IDENT) {
    x = do_ident_global(env, node);
    if (x->f) {
      f_do f = (f_do) x->f;
      free_node(x);
      return f(env, alist);
    }
    if (x->t != NODE_LAMBDA || x->t == NODE_ERROR) {
      free_node(x);
      x = look_func(env, node->s);
      if (!x) {
        x = look_macro(env, node->s);
        if (!x) {
          return new_errorn("illegal function call", node);
        }
        macro = 1;
      }
    }
    if (x->t == NODE_LAMBDA) {
      newenv = (ENV*) x->car->p;
      newenv->r++;
    } else if (x->t == NODE_CELL && x->car->cdr) {
      newenv = (ENV*) x->car->cdr->p;
      newenv->r++;
    }
    c = x->cdr->car;
    p = x->cdr->cdr;
  } else if (node->t == NODE_LAMBDA) {
    x = node;
    newenv = (ENV*) x->car->p;
    newenv->r++;
    c = x->cdr->car;
    p = x->cdr->cdr;
    x = node->cdr;
    x->r++;
  } else {
    return new_errorn("malformed arguments", node);
  }

  if (!newenv) newenv = new_env(env);

  while (alist) {
    if (c && (c->t == NODE_IDENT || (c->car && !strcmp("&rest", c->car->s)))) {
      NODE *l = NULL, *rr = NULL, *nc;
      while (alist) {
        if (macro) {
          nn = alist->car;
          nn->r++;
        } else nn = eval_node(env, alist->car);
        if (nn->t == NODE_ERROR) {
          free_env(newenv);
          free_node(x);
          return nn;
        }

        nc = new_node();
        nc->t = NODE_CELL;
        nc->car = nn;
        if (l == NULL) {
          rr = l = nc;
        } else {
          l->cdr = nc;
          l = l->cdr;
        }
        alist = alist->cdr;
      }
      if (rr) add_variable(newenv, c->t == NODE_IDENT ? c->s : c->cdr->car->s, rr);
      break;
    }
    if (macro) {
      nn = alist->car;
      nn->r++;
    } else nn = eval_node(env, alist->car);
    if (nn->t == NODE_ERROR) {
      free_env(newenv);
      free_node(x);
      return nn;
    }
    add_variable(newenv, c->car->s, nn);
    alist = alist->cdr;
    c = c->cdr;
  }
  if (macro) {
    nn = eval_node(newenv, x->cdr->cdr);
    if (nn->t == NODE_BQUOTE) {
      q = eval_node(newenv, nn);
      c = eval_node(newenv, q);
      free_node(q);
    } else
      c = eval_node(newenv, nn);
    free_node(nn);
    free_env(newenv);
    free_node(x);
    return c;
  }
  c = NULL;
  while (p) {
    if (c) free_node(c);
    c = eval_node(newenv, p->car);
    if (c->t == NODE_ERROR) break;
    p = p->cdr;
  }

  free_env(newenv);
  free_node(x);
  if (c) {
    return c;
  }
  return new_node();
}

static NODE*
do_lambda(ENV *env, NODE *alist) {
  NODE *e, *x;

  if (node_narg(alist) < 2) return new_errorn("malformed lambda", alist);

  e = new_node();
  e->t = NODE_ENV;
  e->p = env;
  env->r++;

  x = new_node();
  x->t = NODE_LAMBDA;
  x->car = e;
  x->cdr = alist;
  alist->r++;
  return x;
}

static NODE*
do_eval(ENV *env, NODE *alist) {
  NODE *c, *x;
  if (node_narg(alist) != 1) return new_errorn("malformed eval", alist);
  c = eval_node(env, alist->car);
  x = eval_node(env, c);
  free_node(c);
  return x;
}

static NODE*
do_funcall(ENV *env, NODE *alist) {
  NODE *x;
  if (node_narg(alist) < 2) return new_errorn("malformed funcall", alist);
  x = eval_node(env, alist->car);
  if (x->t == NODE_QUOTE) x = x->car;
  return call_node(env, x, alist->cdr);
}

static NODE*
do_defun(ENV *env, NODE *alist) {
  ENV *global;
  NODE *x;

  if (node_narg(alist) < 3) return new_errorn("malformed defun", alist);

  x = alist->car;
  if (x->t != NODE_IDENT) {
    return new_errorn("invalid identifier", x);
  }
  if (alist->cdr->car->t != NODE_CELL && alist->cdr->car->t != NODE_NIL) {
    return new_errorn("argument is not a list", alist);
  }

  /* TODO: nested function should have env */
#if 0
  e = new_node();
  e->t = NODE_ENV;
  e->p = env;
  env->r++;
  x->cdr = e;
#endif

  global = global_env(env);
  add_function(global, x->s, alist);
  alist->r++;
  x->r++;
  return x;
}

static NODE*
do_defmacro(ENV *env, NODE *alist) {
  NODE *x;

  if (node_narg(alist) != 3) return new_errorn("malformed defmacro", alist);

  x = alist->car;
  if (x->t != NODE_IDENT) {
    return new_errorn("invalid identifier", x);
  }
  add_macro(env, x->s, alist);
  alist->r++;
  x->r++;
  return x;
}

static NODE*
do_progn(ENV *env, NODE *alist) {
  NODE *c;

  c = NULL;
  while (!node_isnull(alist)) {
    if (c) free_node(c);
    c = eval_node(env, alist->car);
    if (c->t == NODE_ERROR) break;
    alist = alist->cdr;
  }
  if (c) {
    return c;
  }
  return new_node();
}

static NODE*
do_dotimes(ENV *env, NODE *alist) {
  ENV *newenv;
  NODE *c, *x, *nn, *err = NULL;
  int i, r;
  long l;

  if (node_narg(alist) < 1) return new_errorn("malformed dotimes", alist);

  x = alist->car;
  l = node_narg(x);
  if (!(l == 2 || l == 3) || !(x->car && x->car->t == NODE_IDENT))
    return new_errorn("malformed dotimes", alist);

  r = int_value(env, x->cdr->car, &err);
  if (err) return err;

  newenv = new_env(env);
  nn = new_node();
  nn->t = NODE_INT;
  add_variable(newenv, x->car->s, nn);

  for (i = 0; i < r; i++) {
    nn->i = i;
    c = do_progn(newenv, alist->cdr);
    if (c->t == NODE_ERROR) {
      free_env(newenv);
      return c;
    }
    free_node(c);
  }

  if (l == 3) {
    nn->i = r;
    c = eval_node(newenv, x->cdr->cdr->car);
    if (!c) c = new_node();
  } else
    c = new_node();
  free_env(newenv);
  return c;
}

static NODE*
do_type_of(ENV *env, NODE *alist) {
  NODE *c;
  const char *p = "unknown";

  if (!alist) return new_errorn("malformed type-of", alist);

  c = eval_node(env, alist->car);
  if (c->t == NODE_ERROR) return c;
  switch (c->t) {
  case NODE_NIL: p = "null"; break;
  case NODE_T: p = "boolean"; break;
  case NODE_INT: p = "int"; break;
  case NODE_DOUBLE: p = "float"; break;
  case NODE_STRING: p = "string"; break;
  case NODE_QUOTE: p = "cons"; break;
  case NODE_BQUOTE: p = "cons"; break;
  case NODE_CELL: p = "cons"; break;
  case NODE_AREF: p = "aref"; break;
  case NODE_LAMBDA: p = "function"; break;
  case NODE_IDENT: p = "symbol"; break;
  case NODE_ENV: p = "environment"; break;
  case NODE_ERROR: p = "error"; break;
  }
  free_node(c);
  c = new_node();
  c->t = NODE_STRING;
  c->s = strdup(p);
  return c;
}

static NODE*
do_getenv(ENV *env, NODE *alist) {
  NODE *c;
  const char *p;
  if (!alist) return new_errorn("malformed getenv", alist);

  c = eval_node(env, alist->car);
  if (c->t != NODE_STRING || !c->s) {
    free_node(c);
    return new_errorn("malformed getenv", alist);
  }
  p = getenv(c->s);
  free_node(c);
  if (p) {
    c = new_node();
    c->t = NODE_STRING;
    c->s = strdup(p);
    return c;
  }
  return new_node();
}

static NODE*
do_cond(ENV *env, NODE *alist) {
  NODE *c, *n;

  n = alist;
  while (!node_isnull(n)) {
    if (!n->car || n->car->t != NODE_CELL)
      return new_errorn("malformed cond", alist);
    c = eval_node(env, n->car->car);
    if (!node_isnull(c)) {
      if (node_narg(n->car->cdr) == 0)
        return c;
      free_node(c);
      return do_progn(env, n->car->cdr);
    }
    free_node(c);
    n = n->cdr;
  }
  return new_node();
}

static NODE*
do_car(ENV *env, NODE *alist) {
  NODE *x, *c;

  if (node_narg(alist) != 1) return new_errorn("malformed car", alist);

  x = eval_node(env, alist->car);
  if (x->t == NODE_QUOTE) {
    free_node(x);
    c = new_node();
    c->t = NODE_IDENT;
    c->s = strdup("quote");
    return c;
  }
  if (x->t != NODE_CELL && x->t != NODE_NIL) {
    free_node(x);
    return new_errorn("argument is not a list", alist);
  }
  if (x->car) {
    c = x->car;
    c->r++;
    free_node(x);
    return c;
  }
  free_node(x);
  return new_node();
}

static NODE*
do_cdr(ENV *env, NODE *alist) {
  NODE *x, *c;

  if (node_narg(alist) != 1) return new_errorn("malformed cdr", alist);

  x = eval_node(env, alist->car);
  if (x->t == NODE_QUOTE) {
    x->t = NODE_CELL;
    return x;
  }
  if (x->t != NODE_CELL && x->t != NODE_NIL) {
    free_node(x);
    return new_errorn("argument is not a list", alist);
  }

  c = x->cdr;
  if (c)
    c->r++;
  else
    c = new_node();
  free_node(x);
  return c;
}

static NODE*
do_rplaca(ENV *env, NODE *alist) {
  NODE *lhs, *rhs;

  if (node_narg(alist) != 2) return new_errorn("malformed rplaca", alist);

  lhs = eval_node(env, alist->car);
  if (lhs->t == NODE_ERROR) return lhs;
  if (lhs->t != NODE_CELL)
    return new_errorn("malformed rplaca", alist);
  rhs = eval_node(env, alist->cdr->car);
  if (rhs->t == NODE_ERROR) {
    free_node(lhs);
    return rhs;
  }
  free_node(lhs->car);
  lhs->car = rhs;
  return lhs;
}

static NODE*
do_rplacd(ENV *env, NODE *alist) {
  NODE *lhs, *rhs;

  if (node_narg(alist) != 2) return new_errorn("malformed rplacd", alist);

  lhs = eval_node(env, alist->car);
  if (lhs->t == NODE_ERROR) return lhs;
  if (lhs->t != NODE_CELL)
    return new_errorn("malformed rplacd", alist);
  rhs = eval_node(env, alist->cdr->car);
  if (rhs->t == NODE_ERROR) {
    free_node(lhs);
    return rhs;
  }
  free_node(lhs->cdr);
  lhs->cdr = rhs;
  return lhs;
}

static NODE*
do_cons(ENV *env, NODE *alist) {
  NODE *c, *lhs, *rhs;

  if (node_narg(alist) != 2) return new_errorn("malformed cons", alist);

  lhs = eval_node(env, alist->car);
  if (lhs->t == NODE_ERROR) return lhs;
  rhs = eval_node(env, alist->cdr->car);
  if (rhs->t == NODE_ERROR) {
    free_node(lhs);
    return rhs;
  }
  c = new_node();
  c->t = NODE_CELL;
  c->car = lhs;
  c->cdr = rhs;
  return c;
}

static NODE*
do_consp(ENV *env, NODE *alist) {
  NODE *x, *c;
  if (node_narg(alist) != 1) return new_errorn("malformed consp", alist);
  x = eval_node(env, alist->car);
  if (x->t == NODE_ERROR) return x;

  c = new_node();
  if (x->t == NODE_CELL) c->t = NODE_T;
  return c;
}

static NODE*
do_length(ENV *env, NODE *alist) {
  NODE *x, *c;

  if (!alist) return new_errorn("malformed length", alist);

  x = eval_node(env, alist->car);
  if (x->t != NODE_CELL && x->t != NODE_NIL && x->t != NODE_STRING) {
    free_node(x);
    return new_errorn("argument is not a list", alist);
  }
  c = new_node();
  c->t = NODE_INT;
  c->i = x->t == NODE_NIL ? 0 : x->t == NODE_STRING ? (long)strlen(x->s) : node_narg(x);
  free_node(x);
  return c;
}

static NODE*
do_concatenate(ENV *env, NODE *alist) {
  NODE *x, *c, *l, *nn;
  BUFFER buf;

  if (node_narg(alist) < 3) return new_errorn("malformed concatenate", alist);

  x = eval_node(env, alist->car);
  if (x->t != NODE_IDENT) {
    free_node(x);
    return new_errorn("first argument is not a quote", alist);
  }
  l = eval_node(env, alist->cdr->car);
  if (l->t != NODE_CELL && l->t != NODE_NIL && l->t != NODE_STRING) {
    free_node(x);
    return new_errorn("argument is not a list", alist);
  }
  c = new_node();
  c->t = !strcmp(x->s, "string") ? NODE_STRING : NODE_CELL;

  buf_init(&buf);

  alist = alist->cdr;
  while (!node_isnull(alist)) {
    if (c->t == NODE_STRING) {
      nn = eval_node(env, alist->car);
      if (nn->t == NODE_ERROR) {
        free_node(c);
        buf_free(&buf);
        c = nn;
        break;
      }
      if (nn->t != NODE_STRING) {
        free_node(c);
        c = new_errorn("argument is not string", nn);
        free_node(nn);
        buf_free(&buf);
        break;
      }
      buf_append(&buf, nn->s);
      free_node(nn);
    }
    alist = alist->cdr;
  }
  if (node_isnull(alist)) c->s = buf.ptr;
  free_node(x);
  free_node(l);
  return c;
}

static NODE*
do_make_string(ENV *env, NODE *alist) {
  NODE *x, *c;

  if (node_narg(alist) != 1) return new_errorn("malformed make-string", alist);

  x = eval_node(env, alist->car);
  if (x->t == NODE_ERROR) return x;
  if (x->t != NODE_INT || x->i < 0) {
    free_node(x);
    return new_errorn("malformed make-string", alist);
  }
  c = new_node();
  c->t = NODE_STRING;
  c->s = (char*)malloc(x->i + 1);
  memset(c->s, ' ', x->i);
  *(c->s + x->i) = 0;
  free_node(x);
  return c;
}

static NODE*
do_make_array(ENV *env, NODE *alist) {
  NODE *x, *c;
  int i, n;

  if (node_narg(alist) != 1) return new_errorn("malformed make-array", alist);

  x = eval_node(env, alist->car);
  if (x->t == NODE_ERROR) return x;
  if (x->t != NODE_INT || x->i < 0) {
    free_node(x);
    return new_errorn("malformed make-array", alist);
  }
  n = x->i;
  free_node(x);

  c = new_node();
  c->t = NODE_CELL;
  x = c;
  for (i = 0; i < n-1; i++) {
    x->cdr = new_node();
    x->cdr->t = NODE_CELL;
    x->cdr->car = new_node();
    x = x->cdr;
  }
  return c;
}

static NODE*
do_aref(ENV *env, NODE *alist) {
  NODE *x, *c;

  if (node_narg(alist) != 2) return new_errorn("malformed aref", alist);

  x = eval_node(env, alist->car);
  if (x->t == NODE_ERROR) return x;
  if (x->t != NODE_CELL) {
    free_node(x);
    return new_errorn("malformed aref", alist);
  }

  x->r++;
  c = new_node();
  c->t = NODE_AREF;
  c->car = alist;
  alist->r++;
  return c;
}

NODE*
load_lisp(ENV *env, const char *fname) {
  NODE *ret, *top;
  SCANNER sv, *s = &sv;
  FILE *fp;

  fp = fopen(fname, "rb");
  if (!fp) {
    return new_errorf("%s", strerror(errno));
  }

  s_file_init(s, fp);

  top = parse_paren(s, PARSE_ANY);
  if (top == NULL) {
    NODE *e = new_node();
    e->t = NODE_ERROR;
    e->s = strdup(s->err);
    s_reset(s);
    fclose(fp);
    return e;
  }

  skip_white(s);
  if (!s_eof(s)) {
    free_node(top);
    fclose(fp);
    return invalid_token(s);
  }
  fclose(fp);

  ret = do_progn(env, top);

  free_node(top);
  return ret;
}

static NODE*
do_load(ENV *env, NODE *alist) {
  NODE *x, *ret;

  if (!alist->car) return new_errorn("malformed load", alist);
  x = eval_node(env, alist->car);
  if (x->t == NODE_ERROR) return x;
  if (x->t != NODE_STRING) {
    free_node(x);
    return new_errorn("malformed load", alist);
  }
  ret = load_lisp(env, x->s);
  if (ret->t == NODE_ERROR) {
    free_node(x);
    return ret;
  }
  free_node(x);
  free_node(ret);
  ret = new_node();
  ret->t = NODE_T;
  return ret;
}

static NODE*
do_apply(ENV *env, NODE *alist) {
  NODE *x, *f, *nn;

  if (node_narg(alist) < 2) return new_errorn("malformed apply", alist);

  x = eval_node(env, alist->cdr->car);
  if (x->t != NODE_CELL) {
    free_node(x);
    return new_errorn("second argument should be list", alist);
  }
  f = eval_node(env, alist->car);
  nn = call_node(env, f, x);
  if (nn->t == NODE_ERROR) {
    return nn;
  }
  return nn;
}

void
add_sym(ENV *env, enum NODE_TYPE t, const char* n, f_do f) {
  ITEM *ni;
  NODE *node;
  node = new_node();
  node->t = t;
  node->s = strdup(n);
  node->f = f;
  ni = (ITEM*)malloc(sizeof(ITEM));
  memset(ni, 0, sizeof(ITEM));
  ni->k = strdup(n);
  ni->v = node;
  env->lf = (ITEM**)realloc(env->lf, sizeof(ITEM*) * (env->nf + 1));
  env->lf[env->nf] = ni;
  env->nf++;
}

static void
add_defaults(ENV *env) {
  add_sym(env, NODE_IDENT, "%", do_mod);
  add_sym(env, NODE_IDENT, "*", do_mul);
  add_sym(env, NODE_IDENT, "+", do_plus);
  add_sym(env, NODE_IDENT, "-", do_minus);
  add_sym(env, NODE_IDENT, "/", do_div);
  add_sym(env, NODE_IDENT, "1+", do_plus1);
  add_sym(env, NODE_IDENT, "1-", do_minus1);
  add_sym(env, NODE_IDENT, "<", do_lt);
  add_sym(env, NODE_IDENT, "<=", do_le);
  add_sym(env, NODE_IDENT, "=", do_eq);
  add_sym(env, NODE_IDENT, ">", do_gt);
  add_sym(env, NODE_IDENT, ">=", do_ge);
  add_sym(env, NODE_IDENT, "apply", do_apply);
  add_sym(env, NODE_IDENT, "car", do_car);
  add_sym(env, NODE_IDENT, "cdr", do_cdr);
  add_sym(env, NODE_IDENT, "concatenate", do_concatenate);
  add_sym(env, NODE_IDENT, "cond", do_cond);
  add_sym(env, NODE_IDENT, "cons", do_cons);
  add_sym(env, NODE_IDENT, "consp", do_consp);
  add_sym(env, NODE_IDENT, "defun", do_defun);
  add_sym(env, NODE_IDENT, "defmacro", do_defmacro);
  add_sym(env, NODE_IDENT, "dotimes", do_dotimes);
  add_sym(env, NODE_IDENT, "eq?", do_eq);
  add_sym(env, NODE_IDENT, "eval", do_eval);
  add_sym(env, NODE_IDENT, "funcall", do_funcall);
  add_sym(env, NODE_IDENT, "if", do_if);
  add_sym(env, NODE_IDENT, "lambda", do_lambda);
  add_sym(env, NODE_IDENT, "length", do_length);
  add_sym(env, NODE_IDENT, "let", do_let);
  add_sym(env, NODE_IDENT, "let*", do_let_s);
  add_sym(env, NODE_IDENT, "list", do_list);
  add_sym(env, NODE_IDENT, "flet", do_flet);
  add_sym(env, NODE_IDENT, "labels", do_labels);
  add_sym(env, NODE_IDENT, "load", do_load);
  add_sym(env, NODE_IDENT, "make-string", do_make_string);
  add_sym(env, NODE_IDENT, "make-array", do_make_array);
  add_sym(env, NODE_IDENT, "aref", do_aref);
  add_sym(env, NODE_IDENT, "mod", do_mod);
  add_sym(env, NODE_IDENT, "and", do_and);
  add_sym(env, NODE_IDENT, "or", do_or);
  add_sym(env, NODE_IDENT, "not", do_not);
  add_sym(env, NODE_IDENT, "evenp", do_evenp);
  add_sym(env, NODE_IDENT, "oddp", do_oddp);
  add_sym(env, NODE_IDENT, "princ", do_princ);
  add_sym(env, NODE_IDENT, "print", do_print);
  add_sym(env, NODE_IDENT, "println", do_println);
  add_sym(env, NODE_IDENT, "progn", do_progn);
  add_sym(env, NODE_IDENT, "quote", do_quote);
  add_sym(env, NODE_IDENT, "rplaca", do_rplaca);
  add_sym(env, NODE_IDENT, "rplacd", do_rplacd);
  add_sym(env, NODE_IDENT, "setq", do_setq);
  add_sym(env, NODE_IDENT, "setf", do_setf);
  add_sym(env, NODE_IDENT, "exit", do_exit);
  add_sym(env, NODE_IDENT, "type-of", do_type_of);
  add_sym(env, NODE_IDENT, "getenv", do_getenv);
  qsort(env->lf, env->nf, sizeof(ITEM*), compare_item);

  load_libs(env);
}

NODE*
eval_node(ENV *env, NODE *node) {
  NODE *c = NULL;
  switch (node->t) {
  case NODE_LAMBDA:
  case NODE_INT:
  case NODE_DOUBLE:
  case NODE_NIL:
  case NODE_T:
  case NODE_STRING:
  case NODE_ENV:
  case NODE_ERROR:
    node->r++;
    return node;
  case NODE_QUOTE:
    c = node->car;
    c->r++;
    return c;
  case NODE_BQUOTE:
    return do_list(env, node);
  case NODE_IDENT:
    return do_ident(env, node);
  case NODE_CELL:
    c = node->car;
    if (!c) {
      return new_node();
    }
    if (c && c->t == NODE_CELL && c->car && c->car->t != NODE_LAMBDA) {
      NODE *r = eval_node(env, c);
      if (r->t == NODE_LAMBDA) {
        c = call_node(env, r, node->cdr);
        free_node(r);
        return c;
      }
      return r;
    }
    if (c->t == NODE_IDENT || c->t == NODE_LAMBDA) {
      c = call_node(env, c, node->cdr);
    }
    if (c == node->car) {
      c->r++;
      return c;
    }
    if (c) return c;
    return new_errorn("illegal function call", node);
  case NODE_AREF:
    {
      NODE *x = node->car;
      int i;
      for (i = 0; i < node->cdr->i; i++) {
        if (!x) break;
        x = x->cdr;
      }
      if (!x) return new_node();
      x = x->car;
      x->r++;
      return x;
    }
    break;
  }

  return new_error("unknown node");
}

int
main(int argc, char* argv[]) {
  ENV *env;
  NODE *node, *ret;
  SCANNER sv, *s = &sv;

  if (argc > 1) {
    int err = 0;
    env = new_env(NULL);
    add_defaults(env);
    ret = load_lisp(env, argv[1]);
    if (ret->t == NODE_ERROR) {
      fprintf(stderr, "cisp: %s\n", ret->s);
      err = 1;
    }
    free_node(ret);
    free_env(env);
    exit(err);
  }

  s_file_init(s, stdin);

  env = new_env(NULL);
  add_defaults(env);
  while (!s_eof(s)) {
    if (isatty(fileno(stdin))) printf("> ");

    node = parse_any(s, PARSE_ANY);
    if (node == NULL) {
      if (s->err) fprintf(stderr, "cisp: %s\n", s->err);
      s_reset(s);
      continue;
    }

    ret = eval_node(env, node);
    if (ret->t == NODE_ERROR) {
      fprintf(stderr, "cisp: %s\n", ret->s);
    } else if (isatty(fileno(stdin))) {
      dump_node(ret);
    }
    free_node(ret);
    free_node(node);
  }
  free_env(env);
  return 0;
}

/* vim:set et sw=2 cino=>2,\:0: */
