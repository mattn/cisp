#define _CRT_SECURE_NO_WARNINGS 1
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <memory.h>
#include <ctype.h>
#include <float.h>
#include <time.h>
#include <limits.h>
#ifndef _MSC_VER
# include <inttypes.h>
# include <unistd.h>
# define _printf_(a,b) __attribute__ ((format (printf, a, b)))
#else
# include <proces.h>
# include <io.h>
# define strdup(x) _strdup(x)
# define isatty(f) _isatty(f)
# define fileno(f) _fileno(f)
# define getpid() _getpid()
# define snprintf(b,n,f,...) _snprintf(b,n,f,__VA_ARGS__)
# define _printf_(a,b)
#endif

#ifdef _WIN32
# include <windows.h>
#endif

#define CISP_MAIN

#include "cisp.h"
#include "parser.h"
#include "util.h"

#define ILLEGAL_FUNCTION_CALL "illegal function call"

static NODE* do_ident_global(ENV *env, NODE *node);
static NODE* do_progn(ENV *env, NODE *alist);

static void
dump_node(NODE *node) {
  BUFFER buf;
  buf_init(&buf);
  print_node(&buf, node, PRINT_QUOTED);
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

static NODE *node_freelist = NULL;

NODE*
new_node() {
  NODE* node;
  if (node_freelist) {
    node = node_freelist;
    node_freelist = node->cdr;
    memset(node, 0, sizeof(NODE));
  } else {
    node = (NODE*)malloc(sizeof(NODE));
    memset(node, 0, sizeof(NODE));
  }
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
  vsnprintf(buf, sizeof(buf)-1, fmt, list);
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
  print_node(&buf, n, PRINT_DEFAULT);
  node = new_error(buf.ptr);
  return node;
}

static ENV *env_freelist = NULL;

ENV*
new_env(ENV *p) {
  ENV* env;
  if (env_freelist) {
    env = env_freelist;
    env_freelist = env->p;
    env->nv = 0;
    env->p = p;
    env->r = 1;
  } else {
    env = (ENV*)malloc(sizeof(ENV));
    memset(env, 0, sizeof(ENV));
    env->p = p;
    env->r = 1;
  }
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
  if (mode == PRINT_DEFAULT) {
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

void
print_node(BUFFER *buf, NODE *node, PRINT_MODE mode) {
  char tmp[DBL_MAX_10_EXP];
  if (!node) {
    buf_append(buf, "nil");
    return;
  }
  switch (node->t) {
  case NODE_CHARACTER:
    if (node->c == '\n')
      buf_append(buf, "#\\return");
    else if (node->c == '\r')
      buf_append(buf, "#\\newline");
    else if (node->c == '\t')
      buf_append(buf, "#\\tab");
    else if (node->c == '\x0c')
      buf_append(buf, "#\\page");
    else {
      snprintf(tmp, sizeof(tmp)-1, "#\\%c", node->c);
      buf_append(buf, tmp);
    }
    break;
  case NODE_INT:
    snprintf(tmp, sizeof(tmp)-1, "%ld", node->i);
    buf_append(buf, tmp);
    break;
  case NODE_DOUBLE:
    snprintf(tmp, sizeof(tmp)-1, "%lf", node->d);
    buf_append(buf, tmp);
    break;
  case NODE_STRING:
    print_str(buf, node, mode);
    break;
  case NODE_IDENT:
  case NODE_SPECIAL:
  case NODE_BUILTINFUNC:
    buf_append(buf, node->s);
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
  case NODE_ERROR:
  case NODE_NIL:
  case NODE_T:
    free((void*)node->s);
    break;
  case NODE_SPECIAL:
  case NODE_BUILTINFUNC:
  case NODE_IDENT:
    break;
  case NODE_ENV:
    free_env(node->p);
    free(node->name);
    break;
  default:
    break;
  }
  node->cdr = node_freelist;
  node_freelist = node;
}

void
free_env(ENV *env) {
  int i;
  env->r--;
  if (env->r > 0) return;
  for (i = 0; i < env->nv; i++) {
    free_node(env->lv[i]->v);
  }
  for (i = 0; i < env->nf; i++) {
    free_node(env->lf[i]->v);
    free((void*)env->lf[i]);
  }
  for (i = 0; i < env->nm; i++) {
    free_node(env->lm[i]->v);
    free((void*)env->lm[i]);
  }
  free((void*)env->lf);
  free((void*)env->lm);
  env->nv = 0;
  env->nf = 0;
  env->nm = 0;
  env->lf = NULL;
  env->lm = NULL;
  env->p = env_freelist;
  env_freelist = env;
}

static int
compare_item(const void *a, const void *b) {
  if ((*((ITEM**)a))->k == (*((ITEM**)b))->k) return 0;
  else if ((*((ITEM**)a))->k < (*((ITEM**)b))->k) return -1;
  else return 1;
}

static INLINE void
add_item(ITEM ***ll, int *nl, const char *k, NODE *v, int do_sort) {
  ITEM *ni = (ITEM*)malloc(sizeof(ITEM));
  memset(ni, 0, sizeof(ITEM));
  ni->k = k;
  ni->v = v;
  *ll = (ITEM**)realloc(*ll, sizeof(ITEM*) * (*nl + 1));
  (*ll)[*nl] = ni;
  (*nl)++;
  if (do_sort)
    qsort(*ll, *nl, sizeof(ITEM*), compare_item);
}

static INLINE ITEM*
find_item_linear(ITEM **ll, int nl, const char *k) {
  int i;
  for (i = 0; i < nl; i++) {
    if (ll[i]->k == k) {
      return ll[i];
    }
  }
  return NULL;
}

static INLINE ITEM*
find_item(ITEM **ll, int nl, const char *k) {
  int left, right, mid;

  left = 0;
  right = nl - 1;
  while (left <= right) {
    mid = (left + right) / 2;
    if (ll[mid]->k == k) {
      return ll[mid];
    } else if (ll[mid]->k < k) {
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

  ni = find_item_linear(env->lv, env->nv, k);
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
  ITEM *ni = find_item_linear(env->lv, env->nv, k);
  int i;
  if (ni) {
    free_node(ni->v);
    ni->v = node;
    return;
  }
  if (env->nv >= env->cv) {
    int newcv = env->cv ? env->cv * 2 : 4;
    env->lv = (ITEM**)realloc(env->lv, sizeof(ITEM*) * newcv);
    for (i = env->cv; i < newcv; i++) {
      env->lv[i] = (ITEM*)malloc(sizeof(ITEM));
    }
    env->cv = newcv;
  }
  ni = env->lv[env->nv++];
  ni->k = k;
  ni->v = node;
}

void
add_function(ENV *env, const char *k, NODE *node) {
  ITEM *ni = find_item(env->lf, env->nf, k);
  if (ni) {
    free_node(ni->v);
    ni->v = node;
    return;
  }
  add_item(&env->lf, &env->nf, k, node, 1);
}

void
add_macro(ENV *env, const char *k, NODE *node) {
  ITEM *ni = find_item(env->lm, env->nm, k);
  if (ni) {
    free_node(ni->v);
    ni->v = node;
    return;
  }
  add_item(&env->lm, &env->nm, k, node, 1);
}

static long
int_value(ENV *env, NODE *node, NODE **err) {
  long r = 0;
  UNUSED(env);
  if (*err) return 0;
  switch (node->t) {
  case NODE_ERROR: *err = node; return 0;
  case NODE_INT: r = node->i; break;
  case NODE_DOUBLE: r = (long)node->d; break;
  default: *err = new_error("malformed number"); break;
  }
  return r;
}

static double
double_value(ENV *env, NODE *node, NODE **err) {
  double r = 0;
  UNUSED(env);
  if (*err) return 0;
  switch (node->t) {
  case NODE_ERROR: *err = node; return 0;
  case NODE_INT: r = (double)node->i; break;
  case NODE_DOUBLE: r = node->d; break;
  default: *err = new_error("malformed number"); break;
  }
  return r;
}

static NODE*
eval_list(ENV *env, NODE *list) {
  NODE *head = NULL, *prev = NULL, *child, *cur;

  while (!node_isnull(list)) {
    child = eval_node(env, list->car);
    if (child->t == NODE_ERROR) {
      free_node(head);
      return child;
    }
    cur = new_node();
    cur->t = NODE_CELL;
    cur->car = child;
    if (prev)
      prev->cdr = cur;
    else
      head = cur;
    prev = cur;
    list = list->cdr;
  }

  if (head)
    return head;

  return new_node();
}

static NODE*
do_plus(ENV *env, NODE *alist) {
  NODE *nn, *c, *err = NULL;

  nn = new_node();
  nn->t = NODE_INT;
  nn->i = 0;

  while (!node_isnull(alist)) {
    c = alist->car;
    if (nn->t == NODE_INT) {
      if (c->t == NODE_DOUBLE) {
        nn->d = double_value(env, nn, &err) + double_value(env, c, &err);
        nn->t = c->t;
      } else
        nn->i += int_value(env, c, &err);
    } else {
      nn->d += double_value(env, c, &err);
    }
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

  c = alist->car;
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

  alist = alist->cdr;
  if (node_isnull(alist)) {
    if (nn->t == NODE_INT)
      nn->i = -nn->i;
    else
      nn->d = -nn->d;
    return nn;
  }

  while (!node_isnull(alist)) {
    c = alist->car;
    if (nn->t == NODE_INT) {
      if (c->t == NODE_DOUBLE) {
        nn->d = double_value(env, nn, &err) - double_value(env, c, &err);
        nn->t = c->t;
      } else
        nn->i -= int_value(env, c, &err);
    } else {
      nn->d -= double_value(env, c, &err);
    }
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
    c = alist->car;
    if (nn->t == NODE_INT) {
      if (c->t == NODE_DOUBLE) {
        nn->d = double_value(env, nn, &err) * double_value(env, c, &err);
        nn->t = c->t;
      } else
        nn->i *= int_value(env, c, &err);
    } else {
      nn->d *= double_value(env, c, &err);
    }
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

  c = alist->car;
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

  alist = alist->cdr;
  if (node_isnull(alist)) {
    if (nn->t == NODE_INT) {
      if (nn->i == 0) {
        free_node(nn);
        return new_error("division by zero");
      }
      nn->i = 1 / nn->i;
    } else {
      if (nn->d == 0.0) {
        free_node(nn);
        return new_error("division by zero");
      }
      nn->d = 1.0 / nn->d;
    }
    return nn;
  }

  while (!node_isnull(alist)) {
    c = alist->car;
    if (nn->t == NODE_INT) {
      if (c->t == NODE_DOUBLE) {
        nn->d = double_value(env, nn, &err) / double_value(env, c, &err);
        nn->t = c->t;
      } else
        nn->i /= int_value(env, c, &err);
    } else {
      nn->d /= double_value(env, c, &err);
    }
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
  UNUSED(env);

  if (node_narg(alist) != 1) return new_errorn("malformed 1+", alist);

  x = alist->car;

  c = new_node();
  c->t = x->t;
  switch (c->t) {
  case NODE_INT: c->i = x->i + 1; break;
  case NODE_DOUBLE: c->d = x->d + 1.0; break;
  default: free_node(c); c = new_error("malformed number"); break;
  }
  return c;
}

static NODE*
do_minus1(ENV *env, NODE *alist) {
  NODE *x, *c;
  UNUSED(env);

  if (node_narg(alist) != 1) return new_errorn("malformed 1-", alist);

  x = alist->car;

  c = new_node();
  c->t = x->t;
  switch (c->t) {
  case NODE_INT: c->i = x->i - 1; break;
  case NODE_DOUBLE: c->d = x->d - 1.0; break;
  default: free_node(c); c = new_error("malformed number"); break;
  }
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
  NODE *x, *c;
  UNUSED(env);

  if (node_narg(alist) != 1) return new_errorn("malformed not", alist);

  x = alist->car;
  c = new_node();
  if (node_isnull(x))
    c->t = NODE_T;
  return c;
}

static NODE*
do_null(ENV *env, NODE *alist) {
  NODE *x, *c;
  UNUSED(env);

  if (node_narg(alist) != 1) return new_errorn("malformed null", alist);

  x = alist->car;
  c = new_node();
  if (node_isnull(x))
    c->t = NODE_T;
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
  long lhs, rhs;

  if (node_narg(alist) != 2) return new_errorn("malformed mod", alist);

  lhs = int_value(env, alist->car, &err);
  rhs = int_value(env, alist->cdr->car, &err);
  if (err)
    return err;

  if (rhs == 0) return new_errorn("malformed mod", alist);

  c = new_node();
  c->t = NODE_INT;
#if LONG_MIN < -LONG_MAX
  c->i = (lhs == LONG_MIN && rhs == -1) ? 0 : lhs % rhs;
#else
  c->i = lhs % rhs;
#endif
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
    if (r > 0) {
      NODE *tail = new_node();
      tail->t = NODE_TAIL;
      tail->car = alist->cdr->car;
      tail->cdr = NULL;
      return tail;
    }
    return new_node();
  }
  {
    NODE *tail = new_node();
    tail->t = NODE_TAIL;
    tail->car = r > 0 ? alist->cdr->car : alist->cdr->cdr->car;
    tail->cdr = NULL;
    return tail;
  }
}

static NODE*
do_incf(ENV *env, NODE *alist) {
  NODE *x, *c = NULL;

  if (node_narg(alist) != 2 || alist->car->t != NODE_IDENT) return new_errorn("malformed incf", alist);
  if (alist->cdr->car->t != NODE_INT && alist->cdr->car->t) return new_errorn("malformed incf", alist);

  x = alist->car;
  if (!x || x->t != NODE_IDENT)
    return new_errorn("invalid identifier", x);

  while (env) {
    ITEM *ni = find_item_linear(env->lv, env->nv, x->s);
    if (ni) {
      if (ni->v->t != NODE_INT && ni->v->t) {
        return new_errorn("invalid type", alist);
      }
      c = eval_node(env, alist->cdr->car);
      if (c->t == NODE_ERROR) return c;

      if (c->t == NODE_INT)
        ni->v->i += c->i;
      else
        ni->v->d += c->d;
      break;
    }
    if (!env->p) {
      break;
    }
    env = env->p;
  }

  if (c == NULL)
    return new_errorn("invalid identifier", x);
  c->r++;
  return c;
}

static NODE*
do_decf(ENV *env, NODE *alist) {
  NODE *x, *c = NULL;

  if (node_narg(alist) != 2 || alist->car->t != NODE_IDENT) return new_errorn("malformed decf", alist);
  if (alist->cdr->car->t != NODE_INT && alist->cdr->car->t) return new_errorn("malformed decf", alist);

  x = alist->car;
  if (!x || x->t != NODE_IDENT)
    return new_errorn("invalid identifier", x);

  while (env) {
    ITEM *ni = find_item_linear(env->lv, env->nv, x->s);
    if (ni) {
      if (ni->v->t != NODE_INT && ni->v->t) {
        return new_errorn("invalid type", alist);
      }
      c = eval_node(env, alist->cdr->car);
      if (c->t == NODE_ERROR) return c;

      if (c->t == NODE_INT)
        ni->v->i -= c->i;
      else
        ni->v->d -= c->d;
      break;
    }
    if (!env->p) {
      break;
    }
    env = env->p;
  }

  if (c == NULL)
    return new_errorn("invalid identifier", x);
  c->r++;
  return c;
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

  lhs = alist->car;
  rhs = alist->cdr->car;
  nn = new_node();
  switch (lhs->t) {
  case NODE_CHARACTER:
    if (lhs->c == rhs->c) {
      nn->t = NODE_T;
    }
    break;
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
  if (err) {
    free_node(nn);
    return err;
  }
  return nn;
}

static NODE*
do_prin1(ENV *env, NODE *alist) {
  NODE *c;
  BUFFER buf;
  UNUSED(env);

  if (node_narg(alist) != 1) return new_errorn("malformed print", alist);

  c = alist->car;

  buf_init(&buf);
  print_node(&buf, c, PRINT_QUOTED);
  puts(buf.ptr);
  buf_free(&buf);

  c->r++;
  return c;
}

static NODE*
do_print(ENV *env, NODE *alist) {
  NODE *c;
  BUFFER buf;
  UNUSED(env);

  if (node_narg(alist) != 1) return new_errorn("malformed print", alist);

  c = alist->car;

  buf_init(&buf);
  print_node(&buf, c, PRINT_QUOTED);
  puts("");
  printf("%s", buf.ptr);
  buf_free(&buf);

  c->r++;
  return c;
}

static NODE*
do_println(ENV *env, NODE *alist) {
  NODE *c;
  BUFFER buf;
  UNUSED(env);

  if (node_narg(alist) != 1) return new_errorn("malformed println", alist);

  c = alist->car;

  buf_init(&buf);
  print_node(&buf, c, PRINT_QUOTED);
  puts(buf.ptr);
  buf_free(&buf);

  c->r++;
  return c;
}

static NODE*
do_princ(ENV *env, NODE *alist) {
  NODE *c;
  BUFFER buf;
  UNUSED(env);

  if (node_narg(alist) != 1) return new_errorn("malformed princ", alist);

  c = alist->car;

  buf_init(&buf);
  print_node(&buf, alist->car, PRINT_DEFAULT);
  printf("%s", buf.ptr);
  buf_free(&buf);

  c->r++;
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
        c = eval_node(env, n->cdr->car);
        if (c->t == NODE_ERROR) {
          free_env(newenv);
          return c;
        }
        add_variable(newenv, n->car->s, c);
      }
    }
    if (star) {
      env = newenv;
      newenv = new_env(env);
    }
    x = x->cdr;
  }
  c = do_progn(newenv, alist->cdr);
  if (c->t == NODE_TAIL) {
      c->cdr = (NODE*)newenv; // Transfer environment ownership
      return c;
  }
  free_env(newenv);
  return c;
}

static NODE*
do_list(ENV *env, NODE *alist) {
  UNUSED(env);

  if (!alist)
    return new_node();
  alist->r++;
  return alist;
}

static NODE*
do_bquote(ENV *env, NODE *alist) {
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
        c = look_ident(env, intern(v->s+1));
      } else {
        expand = 1;
        c = look_ident(env, v->s);
      }
    } else if (bq && v->t == NODE_QUOTE && v->car && v->car->t == NODE_CELL) {
      NODE tmp;
      memset(&tmp, 0, sizeof(NODE));
      tmp.t = NODE_BQUOTE;
      tmp.car = v->car;
      c = do_bquote(env, &tmp);
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
  NODE *x, *c, *n, *e, *nn;

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
    e->name = strdup(n->car->s);
    e->p->r++;

    nn = new_node();
    nn->t = NODE_LAMBDA;
    nn->car = e;
    nn->cdr = n->cdr;
    nn->cdr->r++;

    add_function(newenv, n->car->s, nn);
    x = x->cdr;
  }
  c = do_progn(newenv, alist->cdr);
  if (c && c->t == NODE_TAIL) {
    c->cdr = (NODE*)newenv; // Transfer environment ownership
    return c;
  }
  free_env(newenv);
  if (c) return c;
  return new_node();
}

static NODE*
do_float(ENV *env, NODE *alist) {
  NODE *c, *err = NULL;
  double d;

  if (node_narg(alist) != 1) return new_errorn("malformed float", alist);

  d = double_value(env, alist->car, &err);
  if (err) {
    return err;
  }
  c = new_node();
  c->t = NODE_DOUBLE;
  c->d = d;
  return c;
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
do_error(ENV *env, NODE *alist) {
  UNUSED(env);

  if (node_narg(alist) != 1 || alist->cdr->t != NODE_STRING) return new_errorn("malformed error", alist);

  return new_error(alist->cdr->car->s);
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

  while (!node_isnull(alist)) {
    x = alist->car;
    if (!x || x->t != NODE_IDENT)
      return new_errorn("invalid identifier", x);

    c = alist->cdr;
    if (!c || c->t != NODE_CELL)
      return new_errorn("malformed setq", alist);

    last = eval_node(env, c->car);
    if (last->t == NODE_ERROR) return last;
    while (env) {
      if (!env->p) {
        add_variable(env, x->s, last);
        break;
      }
      ITEM *ni = find_item_linear(env->lv, env->nv, x->s);
      if (ni) {
        free_node(ni->v);
        ni->v = last;
        break;
      }
      env = env->p;
    }
    alist = c->cdr;
  }
  if (!last) return new_node();
  last->r++;
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
        if (z->t == NODE_ERROR) {
          free_node(c);
          free_node(y);
          return z;
        }
        if (z->t != NODE_INT) {
          free_node(c);
          free_node(y);
          free_node(z);
          return new_errorn("malformed setf", alist);
        }
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
        ITEM *ni = find_item_linear(env->lv, env->nv, x->s);
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

  ni = find_item_linear(env->lv, env->nv, alist->s);
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
    ni = find_item_linear(env->lv, env->nv, lhs->s);
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
  NODE *x = NULL, *p = NULL, *c = NULL, *nn = NULL;
  int macro = 0;

  if (node->t == NODE_IDENT) {
    x = look_func(env, node->s);
    if (!x) {
      x = look_macro(env, node->s);
      if (!x) {
        return new_errorn(ILLEGAL_FUNCTION_CALL, node);
      }
      macro = 1;
    }
  } else {
    x = node;
    x->r++;
  }

  if (x->t == NODE_SPECIAL) {
    f_do f = x->f;
    free_node(x);
    return f(env, alist);
  } else if (x->t == NODE_BUILTINFUNC) {
    f_do f = x->f;
    free_node(x);
    return f(env, alist);
  } else if (x->t != NODE_LAMBDA) {
    free_node(x);
    return new_errorn("malformed arguments", node);
  }

  newenv = new_env(x->car->p);
  c = x->cdr->car;
  p = x->cdr->cdr;

  while (alist && c) {
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
    nn = do_progn(newenv, p);
    // Macro body execution (expansion) must happen in newenv.
    // We cannot simply return a tail for the expansion logic itself if it depends on newenv.
    if (nn->t == NODE_TAIL) {
        nn = eval_node(newenv, nn);
    }

    {
       NODE *tail = new_node();
       tail->t = NODE_TAIL;
       tail->car = nn;
       tail->cdr = NULL; // The expansion result should be evaluated in the caller's env
       free_env(newenv);
       free_node(x);
       return tail;
    }
  }
  c = do_progn(newenv, p);

  if (c && c->t == NODE_TAIL) {
      c->cdr = (NODE*)newenv; // Transfer env ownership
      free_node(x);
      return c;
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
  NODE *x;
  if (node_narg(alist) != 1) return new_errorn("malformed eval", alist);
  x = eval_node(env, alist->car);
  return x;
}

static NODE*
do_format(ENV *env, NODE *alist) {
  char tmp[2];
  const char* p;
  NODE *c, *n;
  BUFFER buf;
  UNUSED(env);

  if (node_narg(alist) < 2) return new_errorn("malformed format", alist);
  if (alist->car->t != NODE_T && alist->car->t != NODE_NIL)
      return new_errorn("malformed format", alist);
  if (alist->cdr->car->t != NODE_STRING) return new_errorn("malformed format", alist);

  p = alist->cdr->car->s;
  n = alist->cdr->cdr;

  buf_init(&buf);

  tmp[1] = 0;
  while (*p) {
    if (*p == '~') {
      int r = 0, i;
      char atmp[DBL_MAX_10_EXP];
      p++;

      while (isdigit(*p))
        r = r * 10 + (int) (*p++ - '0');
      if (*p == '~') {
        tmp[0] = '~';
        r = r == 0 ? 1 : r;
        for (i = 0; i < r; i++)
          buf_append(&buf, tmp);
      } else if (*p == '%') {
        tmp[0] = '\n';
        r = r == 0 ? 1 : r;
        for (i = 0; i < r; i++)
          buf_append(&buf, tmp);
      } else if (*p == '&') {
        tmp[0] = '\n';
        r = r == 0 ? 1 : r;
        for (i = 0; i < r; i++)
          buf_append(&buf, tmp);
      } else if (*p == '|') {
        tmp[0] = '\x0c';
        r = r == 0 ? 1 : r;
        for (i = 0; i < r; i++)
          buf_append(&buf, tmp);
      } else if (n != NULL) {
        // TODO:
        //   ~T: Tabulate
        //   ~P: Plural
        //   ~^: Escape Upward
        //   ~<: Left Justification
        //   ~>: Right Justification
        //   ~[~]: Conditional Expression
        //   ~{~}: Iteration
        //   ~(~): Case Conversion
        //   ~*: Goto
        //   ~?: Recursive Processing
        char f = tolower(*p);
        if (f == 'c' && n->car->t == NODE_CHARACTER) {
          snprintf(atmp, sizeof(atmp)-1, "%c", n->car->c);
          buf_append(&buf, atmp);
          n = n->cdr;
        } else if (f == 'd' && n->car->t == NODE_INT) {
          // TODO: mincol , padchar , commachar , comma-interval
          snprintf(atmp, sizeof(atmp)-1, "%ld", n->car->i);
          buf_append(&buf, atmp);
          n = n->cdr;
        } else if (f == 'x' && n->car->t == NODE_INT) {
          // TODO: mincol , padchar , commachar , comma-interval
          snprintf(atmp, sizeof(atmp)-1, "%lx", n->car->i);
          buf_append(&buf, atmp);
          n = n->cdr;
        } else if (f == 'x' && n->car->t == NODE_INT) {
          // TODO: mincol , padchar , commachar , comma-interval
          snprintf(atmp, sizeof(atmp)-1, "%lo", n->car->i);
          buf_append(&buf, atmp);
          n = n->cdr;
        } else if (f == 'b' && n->car->t == NODE_INT) {
          // TODO: mincol , padchar , commachar , comma-interval
          int z;
          for (z = 128; z > 0; z >>= 1) {
            if ((n->car->i & z) == z) buf_append(&buf, "1");
            else buf_append(&buf, "0");
          }
          n = n->cdr;
        } else if (f == 'f' && n->car->t == NODE_INT) {
          // TODO: w , d , k , overflowchar , padchar
          snprintf(atmp, sizeof(atmp)-1, "%lf", (double) n->car->i);
          buf_append(&buf, atmp);
          n = n->cdr;
        } else if (f == 'f' && n->car->t == NODE_DOUBLE) {
          // TODO: w , d , k , overflowchar , padchar
          snprintf(atmp, sizeof(atmp)-1, "%lf", n->car->d);
          buf_append(&buf, atmp);
          n = n->cdr;
        } else if (f == 'e' && n->car->t == NODE_INT) {
          // TODO: w , d , e , k , overflowchar , padchar , exponentchar
          snprintf(atmp, sizeof(atmp)-1, "%le", (double) n->car->i);
          buf_append(&buf, atmp);
          n = n->cdr;
        } else if (f == 'e' && n->car->t == NODE_DOUBLE) {
          // TODO: w , d , e , k , overflowchar , padchar , exponentchar
          snprintf(atmp, sizeof(atmp)-1, "%le", n->car->d);
          buf_append(&buf, atmp);
          n = n->cdr;
        } else if (f == 'g' && n->car->t == NODE_INT) {
          // TODO w , d , e , k , overflowchar , padchar , exponentchar
          snprintf(atmp, sizeof(atmp)-1, "%lg", (double) n->car->i);
          buf_append(&buf, atmp);
          n = n->cdr;
        } else if (f == 'g' && n->car->t == NODE_DOUBLE) {
          // TODO w , d , e , k , overflowchar , padchar , exponentchar
          snprintf(atmp, sizeof(atmp)-1, "%lg", n->car->d);
          buf_append(&buf, atmp);
          n = n->cdr;
        } else if (f == 'a' && n->car->t == NODE_STRING) {
          print_node(&buf, n->car, 0);
          n = n->cdr;
        } else if (f == 's' && n->car->t == NODE_STRING) {
          print_node(&buf, n->car, 1);
          n = n->cdr;
        } else {
          print_node(&buf, n->car, 0);
          n = n->cdr;
        }
      }
    } else {
      tmp[0] = *p;
      buf_append(&buf, tmp);
    }
    p++;
  }
  c = new_node();
  if (alist->car->t == NODE_T) {
    printf("%s", buf.ptr);
    buf_free(&buf);
  } else {
    c->t = NODE_STRING;
    c->s = buf.ptr;
  }
  return c;
}

static NODE*
do_funcall(ENV *env, NODE *alist) {
  if (node_narg(alist) < 1) return new_errorn("malformed funcall", alist);
  return call_node(env, alist->car, alist->cdr);
}

static NODE*
do_defun(ENV *env, NODE *alist) {
  ENV *global;
  NODE *x, *e, *n;

  if (node_narg(alist) < 2) return new_errorn("malformed defun", alist);

  x = alist->car;
  if (!x || x->t != NODE_IDENT) {
    return new_errorn("invalid identifier", x);
  }
  if (!node_isnull(alist->cdr->car) && alist->cdr->car->t != NODE_CELL) {
    return new_errorn("argument is not a list", alist);
  }

  e = new_node();
  e->t = NODE_ENV;
  e->p = env;
  e->name = strdup(x->s);
  env->r++;

  n = new_node();
  n->t = NODE_LAMBDA;
  n->car = e;
  n->cdr = alist->cdr;
  alist->cdr->r++;

  global = global_env(env);
  add_function(global, x->s, n);
  x->r++;
  return x;
}

static NODE*
do_defmacro(ENV *env, NODE *alist) {
  ENV *global;
  NODE *x, *e, *n;

  if (node_narg(alist) < 2) return new_errorn("malformed defmacro", alist);

  x = alist->car;
  if (!x || x->t != NODE_IDENT) {
    return new_errorn("invalid identifier", x);
  }
  if (!node_isnull(alist->cdr->car) && alist->cdr->car->t != NODE_CELL) {
    return new_errorn("argument is not a list", alist);
  }

  e = new_node();
  e->t = NODE_ENV;
  e->p = env;
  e->name = strdup(x->s);
  env->r++;

  n = new_node();
  n->t = NODE_LAMBDA;
  n->car = e;
  n->cdr = alist->cdr;
  alist->cdr->r++;

  global = global_env(env);
  add_macro(global, x->s, n);
  x->r++;
  return x;
}

static NODE*
do_progn(ENV *env, NODE *alist) {
  NODE *c;

  c = NULL;
  while (!node_isnull(alist)) {
    if (c) free_node(c);
    if (node_isnull(alist->cdr)) {
       NODE *tail = new_node();
       tail->t = NODE_TAIL;
       tail->car = alist->car;
       tail->cdr = NULL;
       return tail;
    }
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

  c = eval_node(env, x->cdr->car);
  r = int_value(env, c, &err);
  free_node(c);
  if (err) return err;

  newenv = new_env(env);
  add_variable(newenv, x->car->s, NULL);

  for (i = 0; i < r; i++) {
    nn = new_node();
    nn->t = NODE_INT;
    nn->i = i;
    add_variable(newenv, x->car->s, nn);
    c = do_progn(newenv, alist->cdr);
    if (c->t == NODE_TAIL) {
        c = eval_node(newenv, c);
    }
    if (c->t == NODE_ERROR) {
      free_env(newenv);
      return c;
    }
    free_node(c);
  }

  if (l == 3) {
    nn = new_node();
    nn->t = NODE_INT;
    nn->i = r;
    add_variable(newenv, x->car->s, nn);
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
  UNUSED(env);

  if (node_narg(alist) != 1) return new_errorn("malformed type-of", alist);

  c = alist->car;
  switch (c->t) {
  case NODE_NIL: p = "null"; break;
  case NODE_T: p = "boolean"; break;
  case NODE_CHARACTER: p = "character"; break;
  case NODE_INT: p = "int"; break;
  case NODE_DOUBLE: p = "float"; break;
  case NODE_STRING: p = "string"; break;
  case NODE_QUOTE: p = "cons"; break;
  case NODE_BQUOTE: p = "cons"; break;
  case NODE_CELL: p = "cons"; break;
  case NODE_AREF: p = "aref"; break;
  case NODE_BUILTINFUNC:
  case NODE_SPECIAL:
  case NODE_LAMBDA: p = "function"; break;
  case NODE_IDENT: p = "symbol"; break;
  case NODE_ENV: p = "environment"; break;
  case NODE_ERROR: p = "error"; break;
  case NODE_TAIL: p = "tail"; break;
  }
  c = new_node();
  c->t = NODE_STRING;
  c->s = strdup(p);
  return c;
}

static NODE*
do_sleep(ENV *env, NODE *alist) {
  NODE *err = NULL;
  double d;
  UNUSED(env);

  if (node_narg(alist) != 1) return new_errorn("malformed time", alist);

  d = double_value(env, alist->car, &err);
  if (err) {
    return err;
  }
#ifdef _WIN32
  Sleep((int)(d * 1000));
#else
  usleep((int)(d * 1000000));
#endif
  return new_node();
}

static NODE*
do_time(ENV *env, NODE *alist) {
  NODE *c;
  clock_t start;
  UNUSED(env);

  if (node_narg(alist) != 1) return new_errorn("malformed time", alist);
  start = clock();
  c = eval_node(env, alist->car);
  printf("Elapsed time: %f msecs\n", (double)(clock() - start));
  return c;
}

static NODE*
do_getenv(ENV *env, NODE *alist) {
  NODE *c;
  const char *p;
  UNUSED(env);
  if (node_narg(alist) != 1) return new_errorn("malformed getenv", alist);

  c = alist->car;
  if (c->t != NODE_STRING || !c->s)
    return new_errorn("malformed getenv", alist);
  p = getenv(c->s);
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
    if (c->t == NODE_ERROR) return c;
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
  UNUSED(env);

  if (node_narg(alist) != 1) return new_errorn("malformed car", alist);

  x = alist->car;
  if (x->t == NODE_QUOTE) {
    c = new_node();
    c->t = NODE_IDENT;
    c->s = (char*)intern("quote");
    return c;
  }
  if (x->t != NODE_CELL && x->t != NODE_NIL)
    return new_errorn("argument is not a list", alist);

  c = x->car;
  if (c)
    c->r++;
  else
    c = new_node();
  return c;
}

static NODE*
do_cdr(ENV *env, NODE *alist) {
  NODE *x, *c;
  UNUSED(env);

  if (node_narg(alist) != 1) return new_errorn("malformed cdr", alist);

  x = alist->car;
  if (x->t == NODE_QUOTE) {
    c = new_node();
    c->t = NODE_CELL;
    c->car = x->car;
    c->car->r++;
    return c;
  }
  if (x->t != NODE_CELL && x->t != NODE_NIL)
    return new_errorn("argument is not a list", alist);

  c = x->cdr;
  if (c)
    c->r++;
  else
    c = new_node();
  return c;
}

static NODE*
do_random(ENV *env, NODE *alist) {
  NODE *c;
  UNUSED(env);

  if (node_narg(alist) != 1) return new_errorn("malformed random", alist);
  if (alist->car->t != NODE_INT && alist->car->t != NODE_DOUBLE)
    return new_errorn("malformed random", alist);

  c = new_node();
  c->t = alist->car->t;
  if (c->t == NODE_INT)
    c->i = rand() % alist->car->i;
  else
    c->d = (double)rand()/RAND_MAX;
  return c;
}

static NODE*
do_rplaca(ENV *env, NODE *alist) {
  NODE *lhs, *rhs;
  UNUSED(env);

  if (node_narg(alist) != 2) return new_errorn("malformed rplaca", alist);

  lhs = alist->car;
  if (lhs->t != NODE_CELL)
    return new_errorn("malformed rplaca", alist);
  rhs = alist->cdr->car;
  free_node(lhs->car);
  lhs->car = rhs;
  rhs->r++;
  lhs->r++;
  return lhs;
}

static NODE*
do_rplacd(ENV *env, NODE *alist) {
  NODE *lhs, *rhs;
  UNUSED(env);

  if (node_narg(alist) != 2) return new_errorn("malformed rplacd", alist);

  lhs = alist->car;
  if (lhs->t != NODE_CELL)
    return new_errorn("malformed rplacd", alist);
  rhs = alist->cdr->car;
  free_node(lhs->cdr);
  lhs->cdr = rhs;
  rhs->r++;
  lhs->r++;
  return lhs;
}

static NODE*
do_cons(ENV *env, NODE *alist) {
  NODE *c, *lhs, *rhs;
  UNUSED(env);

  if (node_narg(alist) != 2) return new_errorn("malformed cons", alist);

  lhs = alist->car;
  lhs->r++;
  rhs = alist->cdr->car;
  rhs->r++;
  c = new_node();
  c->t = NODE_CELL;
  c->car = lhs;
  c->cdr = rhs;
  //puts(">-----------------");
  //dump_node(lhs);
  //dump_node(rhs);
  //puts("-----------------<");
  return c;
}

static NODE*
do_consp(ENV *env, NODE *alist) {
  NODE *c;
  UNUSED(env);

  if (node_narg(alist) != 1) return new_errorn("malformed consp", alist);

  c = new_node();
  switch (alist->car->t) {
  case NODE_QUOTE:
  case NODE_BQUOTE:
  case NODE_CELL:
    c->t = NODE_T;
    break;
  default:
    break;
  }
  return c;
}

static NODE*
do_length(ENV *env, NODE *alist) {
  NODE *x, *c;
  UNUSED(env);

  if (node_narg(alist) != 1) return new_errorn("malformed length", alist);

  x = alist->car;
  if (x->t != NODE_CELL && x->t != NODE_NIL && x->t != NODE_STRING)
    return new_errorn("argument is not a list", alist);
  c = new_node();
  c->t = NODE_INT;
  c->i = x->t == NODE_NIL ? 0 : x->t == NODE_STRING ? (long)strlen(x->s) : node_narg(x);
  return c;
}

static NODE*
do_concatenate(ENV *env, NODE *alist) {
  NODE *x, *c, *l, *nn;
  BUFFER buf;
  UNUSED(env);

  if (node_narg(alist) < 3) return new_errorn("malformed concatenate", alist);

  x = alist->car;
  if (x->t != NODE_IDENT)
    return new_errorn("first argument is not a quote", alist);
  l = alist->cdr->car;
  if (l->t != NODE_CELL && l->t != NODE_NIL && l->t != NODE_STRING)
    return new_errorn("argument is not a list", alist);
  c = new_node();
  c->t = !strcmp(x->s, "string") ? NODE_STRING : NODE_CELL;

  buf_init(&buf);

  alist = alist->cdr;
  while (!node_isnull(alist)) {
    if (c->t == NODE_STRING) {
      nn = alist->car;
      if (nn->t != NODE_STRING) {
        free_node(c);
        c = new_errorn("argument is not string", nn);
        buf_free(&buf);
        break;
      }
      buf_append(&buf, nn->s);
    }
    alist = alist->cdr;
  }
  if (node_isnull(alist)) c->s = buf.ptr;
  return c;
}

static NODE*
do_make_string(ENV *env, NODE *alist) {
  NODE *x, *c;
  UNUSED(env);

  if (node_narg(alist) != 1) return new_errorn("malformed make-string", alist);

  x = alist->car;
  if (x->t != NODE_INT || x->i < 0)
    return new_errorn("malformed make-string", alist);
  c = new_node();
  c->t = NODE_STRING;
  c->s = (char*)malloc(x->i + 1);
  memset(c->s, ' ', x->i);
  *(c->s + x->i) = 0;
  return c;
}

static NODE*
do_make_array(ENV *env, NODE *alist) {
  NODE *x, *c;
  int i, n;
  UNUSED(env);

  if (node_narg(alist) != 1) return new_errorn("malformed make-array", alist);

  x = alist->car;
  if (x->t != NODE_INT || x->i < 0)
    return new_errorn("malformed make-array", alist);
  n = x->i;

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
do_nconc(ENV *env, NODE *alist) {
  NODE *x, *c, *n = NULL, *head = NULL;
  UNUSED(env);

  if (node_isnull(alist))
    return new_node();

  for (c = alist; !node_isnull(c->cdr); c = c->cdr) {
    x = c->car;
    if (!node_isnull(x) && x->t != NODE_CELL)
      return new_errorn("malformed nconc", alist);
  }

  for (c = alist; !node_isnull(c); c = c->cdr) {
    x = c->car;
    if (node_isnull(x) && !node_isnull(c->cdr))
      continue;

    if (head) {
      while (n->cdr && n->cdr->t == NODE_CELL)
        n = n->cdr;
      free_node(n->cdr);
      n->cdr = x;
    } else
      head = n = x;
    x->r++;

  }

  return head;
}

static NODE*
do_aref(ENV *env, NODE *alist) {
  NODE *x, *c;
  UNUSED(env);

  if (node_narg(alist) != 2) return new_errorn("malformed aref", alist);

  x = alist->car;
  if (x->t != NODE_CELL)
    return new_errorn("malformed aref", alist);

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
  if (ret && ret->t == NODE_TAIL) {
      NODE *tail_car = ret->car;
      if (tail_car) tail_car->r++;
      free_node(ret);
      ret = eval_node(env, tail_car); // Evaluate the tail's content
  }
  
  free_node(top);
  return ret;
}

static NODE*
do_load(ENV *env, NODE *alist) {
  NODE *x, *ret;

  if (node_narg(alist) != 1) return new_errorn("malformed load", alist);
  x = alist->car;
  if (x->t != NODE_STRING)
    return new_errorn("malformed load", alist);
  ret = load_lisp(env, x->s);
  if (ret->t == NODE_ERROR)
    return ret;
  free_node(ret);
  ret = new_node();
  ret->t = NODE_T;
  return ret;
}

static NODE*
do_apply(ENV *env, NODE *alist) {
  NODE *c, *nn, *x = NULL, *head = NULL;

  if (node_narg(alist) < 2) return new_errorn("malformed apply", alist);

  for (c = alist->cdr; !node_isnull(c->cdr); c = c->cdr) {
    nn = new_node();
    nn->t = NODE_CELL;
    nn->car = c->car;
    c->car->r++;
    if (head) {
      x->cdr = nn;
      x = nn;
    } else
      head = x = nn;
  }

  if (!node_isnull(c->car) && c->car->t != NODE_CELL) {
    free_node(head);
    return new_errorn("last argument should be list", alist);
  }
  if (head)
    x->cdr = c->car;
  else
    head = c->car;
  c->car->r++;

  nn = call_node(env, alist->car, head);
  free_node(head);
  return nn;
}

static NODE*
do_while(ENV *env, NODE *alist) {
  ENV *newenv;
  NODE *c, *err = NULL;
  int r;

  if (node_narg(alist) < 1) return new_errorn("malformed while", alist);

  newenv = new_env(env);

  while (1) {
    c = eval_node(env, alist->car);
    r = c->t == NODE_T;
    free_node(c);
    if (err) return err;
    if (r == 0) break;

    c = do_progn(newenv, alist->cdr);
    if (c->t == NODE_TAIL) {
      c = eval_node(newenv, c);
    }
    if (c->t == NODE_ERROR) {
      free_env(newenv);
      return c;
    }
    free_node(c);
  }

  c = new_node();
  free_env(newenv);
  return c;
}

void
add_sym(ENV *env, NODE_TYPE t, const char* n, f_do f) {
  ITEM *ni;
  NODE *node;
  node = new_node();
  node->t = t;
  node->s = (char*)intern(n);
  node->f = f;
  ni = (ITEM*)malloc(sizeof(ITEM));
  memset(ni, 0, sizeof(ITEM));
  ni->k = intern(n);
  ni->v = node;
  env->lf = (ITEM**)realloc(env->lf, sizeof(ITEM*) * (env->nf + 1));
  env->lf[env->nf] = ni;
  env->nf++;
}

void
sort_syms(ENV *env) {
  qsort(env->lf, env->nf, sizeof(ITEM*), compare_item);
}

static void
add_defaults(ENV *env) {
  add_sym(env, NODE_BUILTINFUNC, "%", do_mod);
  add_sym(env, NODE_BUILTINFUNC, "*", do_mul);
  add_sym(env, NODE_BUILTINFUNC, "+", do_plus);
  add_sym(env, NODE_BUILTINFUNC, "-", do_minus);
  add_sym(env, NODE_BUILTINFUNC, "/", do_div);
  add_sym(env, NODE_BUILTINFUNC, "1+", do_plus1);
  add_sym(env, NODE_BUILTINFUNC, "1-", do_minus1);
  add_sym(env, NODE_BUILTINFUNC, "<", do_lt);
  add_sym(env, NODE_BUILTINFUNC, "<=", do_le);
  add_sym(env, NODE_BUILTINFUNC, "=", do_eq);
  add_sym(env, NODE_BUILTINFUNC, ">", do_gt);
  add_sym(env, NODE_BUILTINFUNC, ">=", do_ge);
  add_sym(env, NODE_SPECIAL    , "and", do_and);
  add_sym(env, NODE_BUILTINFUNC, "apply", do_apply);
  add_sym(env, NODE_BUILTINFUNC, "aref", do_aref);
  add_sym(env, NODE_BUILTINFUNC, "car", do_car);
  add_sym(env, NODE_BUILTINFUNC, "cdr", do_cdr);
  add_sym(env, NODE_BUILTINFUNC, "concatenate", do_concatenate);
  add_sym(env, NODE_SPECIAL    , "cond", do_cond);
  add_sym(env, NODE_BUILTINFUNC, "cons", do_cons);
  add_sym(env, NODE_BUILTINFUNC, "consp", do_consp);
  add_sym(env, NODE_SPECIAL    , "decf", do_decf);
  add_sym(env, NODE_SPECIAL    , "defmacro", do_defmacro);
  add_sym(env, NODE_SPECIAL    , "defun", do_defun);
  add_sym(env, NODE_SPECIAL    , "dotimes", do_dotimes);
  add_sym(env, NODE_BUILTINFUNC, "eq?", do_eq);
  add_sym(env, NODE_BUILTINFUNC, "eval", do_eval);
  add_sym(env, NODE_BUILTINFUNC, "evenp", do_evenp);
  add_sym(env, NODE_BUILTINFUNC, "error", do_error);
  add_sym(env, NODE_BUILTINFUNC, "exit", do_exit);
  add_sym(env, NODE_SPECIAL    , "flet", do_flet);
  add_sym(env, NODE_BUILTINFUNC, "float", do_float);
  add_sym(env, NODE_BUILTINFUNC, "format", do_format);
  add_sym(env, NODE_BUILTINFUNC, "funcall", do_funcall);
  add_sym(env, NODE_BUILTINFUNC, "getenv", do_getenv);
  add_sym(env, NODE_SPECIAL    , "if", do_if);
  add_sym(env, NODE_SPECIAL    , "incf", do_incf);
  add_sym(env, NODE_SPECIAL    , "labels", do_labels);
  add_sym(env, NODE_SPECIAL    , "lambda", do_lambda);
  add_sym(env, NODE_BUILTINFUNC, "length", do_length);
  add_sym(env, NODE_SPECIAL    , "let", do_let);
  add_sym(env, NODE_SPECIAL    , "let*", do_let_s);
  add_sym(env, NODE_BUILTINFUNC, "list", do_list);
  add_sym(env, NODE_BUILTINFUNC, "load", do_load);
  add_sym(env, NODE_BUILTINFUNC, "make-array", do_make_array);
  add_sym(env, NODE_BUILTINFUNC, "make-string", do_make_string);
  add_sym(env, NODE_BUILTINFUNC, "mod", do_mod);
  add_sym(env, NODE_BUILTINFUNC, "nconc", do_nconc);
  add_sym(env, NODE_BUILTINFUNC, "not", do_not);
  add_sym(env, NODE_BUILTINFUNC, "null", do_null);
  add_sym(env, NODE_BUILTINFUNC, "oddp", do_oddp);
  add_sym(env, NODE_SPECIAL    , "or", do_or);
  add_sym(env, NODE_BUILTINFUNC, "princ", do_princ);
  add_sym(env, NODE_BUILTINFUNC, "print", do_print);
  add_sym(env, NODE_BUILTINFUNC, "prin1", do_prin1);
  add_sym(env, NODE_BUILTINFUNC, "println", do_println);
  add_sym(env, NODE_SPECIAL    , "progn", do_progn);
  add_sym(env, NODE_SPECIAL    , "quote", do_quote);
  add_sym(env, NODE_BUILTINFUNC, "random", do_random);
  add_sym(env, NODE_BUILTINFUNC, "rplaca", do_rplaca);
  add_sym(env, NODE_BUILTINFUNC, "rplacd", do_rplacd);
  add_sym(env, NODE_SPECIAL    , "setf", do_setf);
  add_sym(env, NODE_SPECIAL    , "setq", do_setq);
  add_sym(env, NODE_BUILTINFUNC, "sleep", do_sleep);
  add_sym(env, NODE_BUILTINFUNC, "type-of", do_type_of);
  add_sym(env, NODE_BUILTINFUNC, "time", do_time);
  add_sym(env, NODE_SPECIAL,     "while", do_while);
  sort_syms(env);

  load_libs(env);
}

static NODE*
eval_node_dispatch(ENV *env, NODE *node) {
  NODE *c = NULL;
  switch (node->t) {
  case NODE_TAIL:
    node->r++;
    return node;
  case NODE_LAMBDA:
  case NODE_BUILTINFUNC:
  case NODE_SPECIAL:
  case NODE_CHARACTER:
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
    return do_bquote(env, node);
  case NODE_IDENT:
    return do_ident(env, node);
  case NODE_CELL:
    c = node->car;
    if (!c) {
      return new_errorn(ILLEGAL_FUNCTION_CALL, node);
    }
    if (c->t == NODE_CELL && c->car && c->car->t == NODE_IDENT && !strcmp(c->car->s, "lambda")) {
      NODE *r = do_lambda(env, c->cdr);
      if (r->t != NODE_LAMBDA) {
        free_node(r);
        return new_errorn(ILLEGAL_FUNCTION_CALL, node);
      }
      c = call_node(env, r, node->cdr);
      free_node(r);
    } else if (c->t == NODE_IDENT) {
      NODE *r = look_func(env, c->s);
      if (r && r->t == NODE_BUILTINFUNC) {
        NODE *x = eval_list(env, node->cdr);
        if (x->t == NODE_ERROR) {
          free_node(r);
          return x;
        }
        c = call_node(env, r, x);
        free_node(x);
      } else
        c = call_node(env, c, node->cdr);
      free_node(r);
    } else {
      return new_errorn(ILLEGAL_FUNCTION_CALL, node);
    }
    if (c) return c;
    return new_node();
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

NODE*
eval_node(ENV *env, NODE *node) {
  NODE *ret = NULL;
  ENV *curr_env = env;
  NODE *curr_node = node;
  ENV **env_stack = NULL;
  int env_stack_size = 0;
  int env_stack_capacity = 0;

  while (1) {
    ret = eval_node_dispatch(curr_env, curr_node);
    if (ret && ret->t == NODE_TAIL) {
      NODE *next_node = ret->car;
      ENV *next_env = (ENV*)ret->cdr;

      if (next_node) next_node->r++;

      ret->car = NULL;
      ret->cdr = NULL;
      free_node(ret);

      if (curr_node != node) {
        free_node(curr_node);
      }
      curr_node = next_node;

      if (next_env) {
        // Add to environment stack instead of freeing immediately
        if (env_stack_size >= env_stack_capacity) {
          env_stack_capacity = env_stack_capacity == 0 ? 4 : env_stack_capacity * 2;
          env_stack = (ENV**)realloc(env_stack, sizeof(ENV*) * env_stack_capacity);
        }
        env_stack[env_stack_size++] = next_env;
        curr_env = next_env;
      }
      continue;
    }

    // Free all environments in the stack
    for (int i = 0; i < env_stack_size; i++) {
      free_env(env_stack[i]);
    }
    free(env_stack);

    if (curr_node != node) free_node(curr_node);
    return ret;
  }
}

int
main(int argc, char* argv[]) {
  ENV *env;
  NODE *node, *ret;
  SCANNER sv, *s = &sv;

  srand(time(NULL)^getpid());

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
      if (!isatty(fileno(stdin))) break;
      if (s->err) fprintf(stderr, "cisp: %s\n", s->err);
      s_reset(s);
      continue;
    }

    ret = eval_node(env, node);
    if (ret->t == NODE_ERROR) {
      fprintf(stderr, "cisp: %s\n", ret->s);
    } else if (isatty(fileno(stdin))) {
      puts("");
      dump_node(ret);
    }
    free_node(ret);
    free_node(node);
  }
  free_env(env);
  return 0;
}

/* vim:set et sw=2 cino=>2,\:0: */
