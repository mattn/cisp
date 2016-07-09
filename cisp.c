#define _CRT_SECURE_NO_WARNINGS 1
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <memory.h>
#include <io.h>
#include <ctype.h>

#define SYMBOL_CHARS "+-*/<>=&%"

#define strdup _strdup
#define isatty(x) 1

enum T {
  NODE_NIL, NODE_T, NODE_INT, NODE_DOUBLE, NODE_STRING, NODE_QUOTE, NODE_IDENT,
  NODE_LAMBDA, NODE_CELL, NODE_ERROR,
};

typedef struct _NODE {
  int t;
  union {
    long i;
    double d;
    char* s;
  } u;
  struct _NODE *car;
  struct _NODE *cdr;
  void *f;
  int r;
  int n;
} NODE;

typedef struct {
  const char *k;
  NODE *v;
} ITEM;

typedef struct _ENV {
  int nv;
  ITEM **lv;
  int nf;
  ITEM **lf;
  struct _ENV *p;
} ENV;

typedef size_t(*f_reader)(char*, size_t, size_t, void*);

typedef struct _SCANNER {
  void *v;
  f_reader f;
} SCANNER;

typedef NODE* (*f_do)(ENV*, NODE*);

static char*
raisef(const char* msg, const char *p) {
  if (!p) return NULL;
  fprintf(stderr, "%s: %s\n", msg, p);
  return NULL;
}

static char*
raise(const char *p) {
  if (!p) return NULL;
  fprintf(stderr, "invalid token: %s\n", p);
  return NULL;
}

static const char* parse_any(NODE *node, const char *p);
static const char* parse_ident(NODE *node, const char *p);
static NODE* eval_node(ENV *env, NODE *node);
static void print_node(size_t nbuf, char *buf, NODE *node, int mode);
static void free_node(NODE *node);

static const char*
skip_white(const char *p) {
  if (!p) return NULL;
  while (*p) {
    if (*p == ';') {
      p++;
      while (*p && *p != '\n') p++;
    }
    else if (isspace((int)*p)) p++;
    else break;
  }
  return p;
}

static NODE*
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
  node->u.s = strdup(msg);
  return node;
}

static NODE*
new_errorn(const char* fmt, NODE *n) {
  NODE* node;
  char buf[BUFSIZ], tmp[BUFSIZ];
  tmp[0] = 0;
  print_node(sizeof(tmp), tmp, n, 0);
  snprintf(buf, sizeof(buf), fmt, tmp);
  node = new_node();
  node->t = NODE_ERROR;
  node->u.s = strdup(buf);
  return node;
}

static NODE*
new_errorf(const char* fmt, ...) {
  char buf[BUFSIZ];
  va_list list;
  va_start(list, fmt);
  vsnprintf(buf, sizeof(buf), fmt, list);
  va_end(list);
  NODE* node = new_node();
  node->t = NODE_ERROR;
  node->u.s = strdup(buf);
  return node;
}

static ENV*
new_env(ENV *p) {
  ENV* env = (ENV*)malloc(sizeof(ENV));
  memset(env, 0, sizeof(ENV));
  env->p = p;
  return env;
}

static int
node_narg(NODE *node) {
  int i = 0;
  while (node->cdr) {
    node = node->cdr;
    i++;
  }
  return i;
}

static void
node_append(NODE *node, NODE *c) {
  while (node->cdr) node = node->cdr;
  node->cdr = c;
}

static void
node_set_nth(NODE *node, int n, NODE *rhs) {
  int i = 0;
  while (node) {
    if (i == n) {
      node->cdr = rhs;
      return;
    }
    node = node->cdr;
    i++;
  }
}
static NODE*
node_nth(NODE *node, int n) {
  int i = 0;
  while (node) {
    node = node->cdr;
    if (i == n) return node;
    i++;
  }
  return NULL;
}

static int
match(const char *lhs, const char *rhs, size_t n) {
  const char *p = lhs, *e = lhs + n;
  while (p < e) if (*p++ != *rhs++) return 0;
  if (*p || *rhs) return 0;
  return 1;
}

static const char*
parse_number(NODE *node, const char* p) {
  const char *t = p;
  if (*p == '-') p++;
  while (*p && isdigit(*p)) p++;

  if (t == p) {
    return parse_ident(node, t);
  }
  if (*p == '.') {
    p++;
    while (*p && isdigit(*p)) p++;
    node->t = NODE_DOUBLE;
    node->u.d = atof(t);
    return p;
  }

  if (*(p - 1) == '-' || strchr(SYMBOL_CHARS, *p)) {
    return parse_ident(node, t);
  }
  node->t = NODE_INT;
  node->u.i = atoi(t);
  return p;
}

static const char*
parse_paren(NODE *node, const char *p) {
  if (!p) return NULL;
  p = skip_white(p);

  NODE *head = node;
  while (p && *p && *p != ')') {
    NODE *child = new_node();
    p = parse_any(child, p);
    if (!p) {
      free_node(child);
      return NULL;
    }

    if (node == head) {
      if (node->t == NODE_NIL) node->t = NODE_CELL;
      node->car = child;
      node = node->car;
    }
    else {
      node->cdr = child;
      node = node->cdr;
    }

    p = skip_white(p);
    if (*p == '.') {
      node->t = NODE_CELL;
      NODE *cdr = new_node();
      p = parse_any(cdr, p + 1);
      if (p) {
        node->cdr = cdr;
        node = cdr;
      }
      break;
    }
  }
  if (p && *p) {
    if (*p == ')') p++;
    else return raisef("shoud be )", p);
  }
  return p;
}

static const char*
parse_ident(NODE *node, const char *p) {
  const char *t = p;
  while (*p && (isalpha(*p) || isdigit(*p) || strchr(SYMBOL_CHARS, *p))) p++;
  if (match(t, "nil", (size_t)(p - t))) {
    node->t = NODE_NIL;
    return p;
  }
  if (match(t, "t", (size_t)(p - t))) {
    node->t = NODE_T;
    return p;
  }
  node->t = NODE_IDENT;
  node->u.s = (char*)malloc((size_t)(p - t) + 1);
  memset(node->u.s, 0, (size_t)(p - t) + 1);
  memcpy(node->u.s, t, (size_t)(p - t));
  return p;
}

static const char*
parse_quote(NODE *node, const char *p) {
  NODE *child = new_node();
  if (*p == '(') {
    p = parse_paren(child, p + 1);
  }
  else {
    p = parse_ident(child, p);
  }
  if (!p) {
    free_node(child);
    return NULL;
  }
  node->t = NODE_QUOTE;
  node->car = child;
  return p;
}

static const char*
parse_string(NODE *node, const char *p) {
  const char *t = p;
  char *sp;
  int n = 0;
  while (*p) {
    if (*p == '\\' && *(p + 1)) p++;
    else if (*p == '"') break;
    p++;
    n++;
  }
  node->u.s = (char*)malloc(n + 1);
  memset(node->u.s, 0, n + 1);
  sp = node->u.s;
  while (*t) {
    if (*t == '\\' && *(t + 1)) t++;
    else if (*t == '"') break;
    *sp++ = *t++;
  }

  p++;
  node->t = NODE_STRING;
  return p;
}

static const char*
parse_any(NODE *node, const char *p) {
  if (!p) return NULL;
  p = skip_white(p);
  if (!*p) return p;
  if (*p == '(') return parse_paren(node, p + 1);
  if (*p == '-' || isdigit(*p)) return parse_number(node, p);
  if (*p == '\'') return parse_quote(node, p + 1);
  if (*p == '"') return parse_string(node, p + 1);
  if (isalpha(*p) || strchr(SYMBOL_CHARS, *p)) return parse_ident(node, p);
  if (*p) return raise(p);
  return p;
}

static void
print_args(size_t nbuf, char *buf, NODE *node, int mode) {
  NODE *p = node;
  do {
    if (p != node) strncat(buf, " ", nbuf);
    print_node(nbuf, buf, node, mode);
    node = node->cdr;
  } while (node);
}

static void
print_cell(size_t nbuf, char *buf, NODE *node, int mode) {
  strncat(buf, "(", nbuf);
  for (;;) {
    if (node->car) {
      print_node(nbuf, buf, node->car, mode);
    }
    if (!node->cdr)
      break;
    if (node->cdr->t != NODE_CELL) {
      if (node->t != NODE_CELL) strncat(buf, " . ", nbuf);
      print_node(nbuf, buf, node->cdr, mode);
    }
    strncat(buf, " ", nbuf);
    node = node->cdr;
  }
  strncat(buf, ")", nbuf);
}

#if 0
static void
print_list(size_t nbuf, char *buf, NODE *node, int mode) {
  NODE *p = node;
  do {
    if (p != node) strncat(buf, " ", nbuf);
    print_node(nbuf, buf, p, mode);
    p = p->car;
  } while (p);
}
#endif

static void
print_str(size_t nbuf, char *buf, NODE *node, int mode) {
  if (mode) {
    strncat(buf, node->u.s, nbuf);
    return;
  }
  const char* p = node->u.s;
  char tmp[2];
  tmp[1] = 0;
  strncat(buf, "\"", nbuf);
  while (*p) {
    if (*p == '\\') strncat(buf, "\\", nbuf);
    tmp[0] = *p;
    strncat(buf, tmp, nbuf);
    p++;
  }
  strncat(buf, "\"", nbuf);
}

static void
print_float(size_t nbuf, char *buf, NODE *node) {
  char tmp[BUFSIZ];
  snprintf(tmp, sizeof(tmp), "%lf", node->u.d);
  if (node->u.d == (double)(int)(node->u.d)) {
    char *p = tmp + strlen(tmp) - 1;
    while (p > tmp && *(p - 1) == '0') *p-- = 0;
  }
  strncat(buf, tmp, nbuf);
}

static void
print_node(size_t nbuf, char* buf, NODE *node, int mode) {
  char tmp[BUFSIZ];
  switch (node->t) {
  case NODE_INT: snprintf(tmp, sizeof(tmp), "%ld", node->u.i); strncat(buf, tmp, nbuf); break;
  case NODE_DOUBLE: print_float(nbuf, buf, node); break;
  case NODE_STRING: print_str(nbuf, buf, node, mode); break;
  case NODE_IDENT: snprintf(tmp, sizeof(tmp), "%s", node->u.s); strncat(buf, tmp, nbuf); break;
  case NODE_NIL: strncat(buf, "nil", nbuf); break;
  case NODE_T: strncat(buf, "t", nbuf); break;
  case NODE_QUOTE: strncat(buf, "'", nbuf); print_node(nbuf, buf, node->car, mode); break;
  case NODE_CELL: print_cell(nbuf, buf, node, mode); break;
  case NODE_LAMBDA: strncat(buf, "(lambda", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  default: strncat(buf, "()", nbuf); break;
  }
}

static void
free_node(NODE *node) {
  if (!node) return;
  node->r--;
  if (node->r <= 0) {
    switch (node->t) {
    case NODE_STRING:
    case NODE_IDENT:
    case NODE_CELL:
    case NODE_LAMBDA:
    case NODE_ERROR:
    case NODE_NIL:
    case NODE_T:
        free((void*)node->u.s);
        break;
    }
    NODE *car = node->car;
    free((void*)node);
    free_node(car);
  }
}

static void
free_env(ENV *env) {
  int i;
  for (i = 0; i < env->nv; i++) {
    free((void*)env->lv[i]->k);
    free_node(env->lv[i]->v);
    free((void*)env->lv[i]);
  }
  for (i = 0; i < env->nf; i++) {
    free((void*)env->lf[i]->k);
    free_node(env->lf[i]->v);
    free((void*)env->lf[i]);
  }
  free((void*)env->lv);
  free((void*)env->lf);
  free((void*)env);
}

static void
add_variable(ENV *env, const char *k, NODE *node) {
  int i;
  ITEM *ni;
  node->r++;
  for (i = 0; i < env->nv; i++) {
    if (!strcmp(env->lv[i]->k, k)) {
      free_node(env->lv[i]->v);
      env->lv[i]->v = node;
      return;
    }
  }
  ni = (ITEM*)malloc(sizeof(ITEM));
  memset(ni, 0, sizeof(ITEM));
  ni->k = strdup(k);
  ni->v = node;
  env->lv = (ITEM**)realloc(env->lv, sizeof(ITEM*) * (env->nv + 1));
  env->lv[env->nv] = ni;
  env->nv++;
}

static void
add_function(ENV *env, const char *k, NODE *node) {
  int i;
  ITEM *ni;
  node->r++;
  for (i = 0; i < env->nf; i++) {
    if (!strcmp(env->lf[i]->k, k)) {
      free_node(env->lf[i]->v);
      env->lf[i]->v = node;
      return;
    }
  }
  ni = (ITEM*)malloc(sizeof(ITEM));
  memset(ni, 0, sizeof(ITEM));
  ni->k = strdup(k);
  ni->v = node;
  env->lf = (ITEM**)realloc(env->lf, sizeof(ITEM*) * (env->nf + 1));
  env->lf[env->nf] = ni;
  env->nf++;
}

static long
int_value(ENV *env, NODE *node, NODE **err) {
  if (*err) return 0;
  node = eval_node(env, node);
  int r = 0;
  switch (node->t) {
  case NODE_ERROR: *err = node; return 0;
  case NODE_NIL: r = 0; break;
  case NODE_T: r = 1; break;
  case NODE_INT: r = node->u.i; break;
  case NODE_DOUBLE: r = (long)node->u.d; break;
  case NODE_QUOTE: r = int_value(env, node->car, err); break;
  default: *err = new_errorf("malformed number"); break;
  }
  free_node(node);
  return r;
}

static double
double_value(ENV *env, NODE *node, NODE **err) {
  if (*err) return 0;
  node = eval_node(env, node);
  double r = 0;
  switch (node->t) {
  case NODE_ERROR: *err = node; return 0;
  case NODE_INT: r = (double)node->u.i; break;
  case NODE_DOUBLE: r = node->u.d; break;
  case NODE_QUOTE: r = double_value(env, node->car, err); break;
  default: *err = new_errorf("malformed number"); break;
  }
  free_node(node);
  return r;
}

static NODE*
do_plus(ENV *env, NODE *node) {
  NODE *nn, *c, *err = NULL;

  if (node_narg(node) < 2) return new_errorn("malformed +: %s", node);

  node = node->cdr;
  c = eval_node(env, node);
  if (c->t == NODE_ERROR) return c;
  nn = new_node();
  nn->t = c->t;
  nn->u = c->u;
  free_node(c);

  node = node->cdr;
  while (node) {
    c = eval_node(env, node);
    if (c->t == NODE_ERROR) {
      free_node(nn);
      return c;
    }
    switch (nn->t) {
    case NODE_INT:
        if (c->t == NODE_DOUBLE) {
          nn->u.d = double_value(env, nn, &err) + double_value(env, c, &err);
          nn->t = c->t;
        }
        else
          nn->u.i += int_value(env, c, &err);
        break;
    case NODE_DOUBLE:
        nn->u.d += double_value(env, c, &err);
        break;
    default:
        err = new_errorf("malformed number");
        break;
    }
    free_node(c);
    if (err) {
      free_node(nn);
      return err;
    }
    node = node->cdr;
  }
  return nn;
}

static NODE*
do_minus(ENV *env, NODE *node) {
  NODE *nn, *c, *err = NULL;

  if (node_narg(node) < 2) return new_errorn("malformed -: %s", node);

  node = node->cdr;
  c = eval_node(env, node);
  if (c->t == NODE_ERROR) return c;
  nn = new_node();
  nn->t = c->t;
  nn->u = c->u;
  free_node(c);

  node = node->cdr;
  while (node) {
    c = eval_node(env, node);
    if (c->t == NODE_ERROR) return c;
    switch (nn->t) {
    case NODE_INT:
        if (c->t == NODE_DOUBLE) {
          nn->u.d = double_value(env, nn, &err) - double_value(env, c, &err);
          nn->t = c->t;
        }
        else
          nn->u.i -= int_value(env, c, &err);
        break;
    case NODE_DOUBLE:
        nn->u.d -= double_value(env, c, &err);
        break;
    default:
        err = new_errorf("malformed number");
        break;
    }
    free_node(c);
    if (err) {
      free_node(nn);
      return err;
    }
    node = node->cdr;
  }
  return nn;
}

static NODE*
do_mul(ENV *env, NODE *node) {
  NODE *nn, *c, *err = NULL;

  if (node_narg(node) < 2) return new_errorn("malformed *: %s", node);

  node = node->cdr;
  c = eval_node(env, node);
  if (c->t == NODE_ERROR) return c;
  nn = new_node();
  nn->t = c->t;
  nn->u = c->u;
  free_node(c);

  node = node->cdr;
  while (node) {
    c = eval_node(env, node);
    if (c->t == NODE_ERROR) return c;
    switch (nn->t) {
    case NODE_INT:
        if (c->t == NODE_DOUBLE) {
          nn->u.d = double_value(env, nn, &err) * double_value(env, c, &err);
          nn->t = c->t;
        }
        else
          nn->u.i *= int_value(env, c, &err);
        break;
    case NODE_DOUBLE:
        nn->u.d *= double_value(env, c, &err);
        break;
    default:
        err = new_errorf("malformed number");
        break;
    }
    free_node(c);
    if (err) {
      free_node(nn);
      return err;
    }
    node = node->cdr;
  }
  return nn;
}

static NODE*
do_div(ENV *env, NODE *node) {
  NODE *nn, *c, *err = NULL;

  if (node_narg(node) < 2) return new_errorn("malformed /: %s", node);

  node = node->cdr;
  c = eval_node(env, node);
  if (c->t == NODE_ERROR) return c;
  nn = new_node();
  nn->t = c->t;
  nn->u = c->u;
  free_node(c);

  node = node->cdr;
  while (node) {
    c = eval_node(env, node);
    if (c->t == NODE_ERROR) return c;
    switch (nn->t) {
    case NODE_INT:
        if (c->t == NODE_DOUBLE) {
          nn->u.d = double_value(env, nn, &err) / double_value(env, c, &err);
          nn->t = c->t;
        }
        else
          nn->u.i /= int_value(env, c, &err);
        break;
    case NODE_DOUBLE:
        nn->u.d /= double_value(env, c, &err);
        break;
    default:
        err = new_errorf("malformed number");
        break;
    }
    free_node(c);
    if (err) {
      free_node(nn);
      return err;
    }
    node = node->cdr;
  }
  return nn;
}

static NODE*
do_plus1(ENV *env, NODE *node) {
  NODE *x, *c;

  if (node_narg(node) != 1) return new_errorn("malformed 1+: %s", node);

  x = eval_node(env, node->cdr);
  if (x->t == NODE_ERROR) return x;

  c = new_node();
  c->t = x->t;
  switch (c->t) {
  case NODE_INT: c->u.i = x->u.i + 1; break;
  case NODE_DOUBLE: c->u.d = x->u.i + 1.0; break;
  default: free_node(c); c = new_errorf("malformed number"); break;
  }
  free_node(x);
  return c;
}

static NODE*
do_minus1(ENV *env, NODE *node) {
  NODE *x, *c;

  if (node_narg(node) != 1) return new_errorn("malformed 1-: %s", node);

  x = eval_node(env, node->cdr);
  if (x->t == NODE_ERROR) return x;

  c = new_node();
  c->t = x->t;
  switch (c->t) {
  case NODE_INT: c->u.i = x->u.i - 1; break;
  case NODE_DOUBLE: c->u.d = x->u.i - 1.0; break;
  default: free_node(c); c = new_errorf("malformed number"); break;
  }
  free_node(x);
  return c;
}

static NODE*
do_not(ENV *env, NODE *node) {
  NODE *c, *err = NULL;

  if (node_narg(node) != 1) return new_errorn("malformed not: %s", node);

  c = new_node();
  c->t = NODE_INT;
  c->u.i = !int_value(env, node->cdr, &err);
  if (err) {
    free_node(c);
    return err;
  }
  return c;
}

static NODE*
do_mod(ENV *env, NODE *node) {
  NODE *c, *err = NULL;

  if (node_narg(node) != 2) return new_errorn("malformed mod: %s", node);

  c = new_node();
  c->t = NODE_INT;
  c->u.i = int_value(env, node->cdr, &err) % int_value(env, node->cdr->cdr, &err);
  if (err) {
    free_node(c);
    return err;
  }
  return c;
}

static NODE*
do_if(ENV *env, NODE *node) {
  NODE *c;
  int r = 0;

  if (node_narg(node) != 3) return new_errorn("malformed if: %s", node);

  c = eval_node(env, node->cdr);
  if (c->t == NODE_ERROR) return c;
  switch (c->t) {
  case NODE_NIL:
      r = 0;
      break;
  case NODE_T:
      r = 1;
      break;
  case NODE_INT:
      r = c->u.i;
      break;
  case NODE_DOUBLE:
      r = (long)c->u.d;
      break;
  default:
      r = 1;
      break;
  }
  free_node(c);
  return eval_node(env, r > 0 ? node->cdr->cdr : node->cdr->cdr->cdr);
}

static NODE*
do_gt(ENV *env, NODE *node) {
  NODE *nn, *err = NULL;

  if (node_narg(node) != 2) return new_errorn("malformed >: %s", node);

  nn = new_node();
  if (double_value(env, node->cdr, &err) > double_value(env, node->cdr->cdr, &err)) {
    nn->t = NODE_T;
  }
  if (err) {
    free_node(nn);
    return err;
  }
  return nn;
}

static NODE*
do_ge(ENV *env, NODE *node) {
  NODE *nn, *err = NULL;

  if (node_narg(node) != 2) return new_errorn("malformed >=: %s", node);

  nn = new_node();
  if (double_value(env, node->cdr, &err) >= double_value(env, node->cdr->cdr, &err)) {
    nn->t = NODE_T;
  }
  if (err) {
    free_node(nn);
    return err;
  }
  return nn;
}

static NODE*
do_lt(ENV *env, NODE *node) {
  NODE *nn, *err = NULL;

  if (node_narg(node) != 2) return new_errorn("malformed <: %s", node);

  nn = new_node();
  if (double_value(env, node->cdr, &err) < double_value(env, node->cdr->cdr, &err)) {
    nn->t = NODE_T;
  }
  if (err) {
    free_node(nn);
    return err;
  }
  return nn;
}

static NODE*
do_le(ENV *env, NODE *node) {
  NODE *nn, *err = NULL;

  if (node_narg(node) != 2) return new_errorn("malformed <=: %s", node);

  nn = new_node();
  if (double_value(env, node->cdr, &err) <= double_value(env, node->cdr->cdr, &err)) {
    nn->t = NODE_T;
  }
  if (err) {
    free_node(nn);
    return err;
  }
  return nn;
}

static NODE*
do_eq(ENV *env, NODE *node) {
  NODE *lhs, *rhs, *nn, *err = NULL;

  if (node_narg(node) != 2) return new_errorn("malformed =: %s", node);

  lhs = eval_node(env, node->cdr);
  rhs = eval_node(env, node->cdr->cdr);
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
      if (rhs->t == NODE_STRING && !strcmp(lhs->u.s, rhs->u.s)) {
        nn->t = NODE_T;
      }
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
do_print(ENV *env, NODE *node) {
  NODE *c;
  char buf[BUFSIZ];

  if (node_narg(node) != 1) return new_errorn("malformed print: %s", node);

  c = eval_node(env, node->cdr);
  if (c->t == NODE_ERROR) return c;
  buf[0] = 0;
  print_node(sizeof(buf), buf, c, 0);
  puts(buf);
  return c;
}

static NODE*
do_println(ENV *env, NODE *node) {
  NODE *c;
  char buf[BUFSIZ];

  if (node_narg(node) != 1) return new_errorn("malformed println: %s", node);

  c = eval_node(env, node->cdr);
  if (c->t == NODE_ERROR) return c;
  buf[0] = 0;
  print_node(sizeof(buf), buf, c, 0);
  puts(buf);
  return c;
}

static NODE*
do_princ(ENV *env, NODE *node) {
  NODE *c;
  char buf[BUFSIZ];

  if (node_narg(node) != 1) return new_errorn("malformed printc: %s", node);

  c = eval_node(env, node->cdr);
  if (c->t == NODE_ERROR) return c;
  buf[0] = 0;
  print_node(sizeof(buf), buf, c, 1);
  printf("%s", buf);
  return c;
}

static NODE*
do_quote(ENV *env, NODE *node) {
  NODE *c;

  if (!node->cdr) return new_errorf("malformed quote");
  c = node->cdr;
  c->r++;
  return c;
}

static NODE*
do_let(ENV *env, NODE *node) {
  ENV *newenv;
  NODE *x, *c;
  int i = 0;

  if (node_narg(node) != 2) return new_errorn("malformed let: %s", node);

  x = node->cdr;
  if (x->t != NODE_CELL)
    return new_errorn("malformed let: %s", node);

  newenv = new_env(env);

  /* TODO */
  while (x) {
    switch (node_nth(x, i)->t) {
    case NODE_CELL:
        if (node_narg(node_nth(x, i)) != 2 || node_nth(node_nth(x, i), 0)->t != NODE_IDENT)
          return new_errorn("malformed let: %s", node);
        c = eval_node(env, node_nth(node_nth(x, i), 1));
        if (x->t == NODE_ERROR) return c;
        add_variable(newenv, node_nth(node_nth(x, i), 0)->u.s, c);
        free_node(c);
        break;
    default:
        return new_errorn("malformed let: %s", node);
    }
  }
  c = NULL;
  for (i = 2; i < node->n; i++) {
    if (c) free_node(c);
    c = eval_node(newenv, node_nth(node, i));
    if (c->t == NODE_ERROR) break;
  }
  free_env(newenv);
  if (c) {
    return c;
  }
  return new_node();
}

static NODE*
do_setq(ENV *env, NODE *node) {
  NODE *x, *c;
  static ENV *global;

  if (node_narg(node) != 2) return new_errorn("malformed setq: %s", node);

  x = node->cdr;
  if (x->t != NODE_IDENT) {
    return new_errorn("invalid identifier: %s", x);
  }

  if (global == NULL) {
    while (env->p) env = env->p;
    global = env;
  }

  c = eval_node(global, node->cdr->cdr);
  if (c->t == NODE_ERROR) return c;
  add_variable(env, x->u.s, c);
  return c;
}

static NODE*
do_ident(ENV *env, NODE *node) {
  NODE *x;
  int i;

  for (i = 0; i < env->nv; i++) {
    if (!strcmp(env->lv[i]->k, node->u.s)) {
      x = env->lv[i]->v;
      x->r++;
      return x;
    }
  }

  if (env->p) return do_ident(env->p, node);
  return new_errorf("unknown variable: %s", node->u.s);
}

static NODE*
look_func(ENV *env, const char *k) {
  NODE *x;
  static ENV *global;
  int i;

  if (!k) return NULL;
  if (global == NULL) {
    while (env->p) env = env->p;
    global = env;
  }
  else {
    env = global;
  }
  for (i = 0; i < env->nf; i++) {
    if (!strcmp(env->lf[i]->k, k)) {
      x = env->lf[i]->v;
      x->r++;
      return x;
    }
  }
  return NULL;
}

static NODE*
do_call(ENV *env, NODE *node) {
  ENV *newenv;
  NODE *f = NULL, *x, *c, *nn;
  int i;
  static ENV *global;

  if (node->t == NODE_IDENT) {
    if (global == NULL) {
      while (env->p) env = env->p;
      global = env;
    }
    for (i = 0; i < global->nv; i++) {
      if (match(node->u.s, global->lv[i]->k, strlen(global->lv[i]->k))) {
        if (global->lv[i]->v->f) {
          return ((f_do)(global->lv[i]->v->f))(env, node);
        }
      }
    }
    f = do_ident(env, node);
    if (f->t != NODE_ERROR) return f;
    free_node(f);
    f = look_func(env, node->u.s);
    if (!f) {
      return new_errorn("malformed arguments: %s", node);
    }
  }
  else if (node->t == NODE_LAMBDA) {
    f = eval_node(env, node);
    f->r++;
  }
  else {
    return new_errorn("malformed arguments: %s", node);
  }

  x = f;
  c = x->cdr->cdr->car;
  newenv = new_env(env);

  node = node->cdr;
  while (node) {
    /*
       if (!strcmp("&rest", c->u.s) || (c->t == NODE_CELL && i == c->n)) {
       l = new_node();
       l->t = NODE_CELL;
       for (j = i; j < node->n; j++) {
       node_nth(node, j)->r++;
       node_append(l, node_nth(node, j));
       l->n++;
       }
       if (c->t == NODE_CELL)
       add_variable(newenv, node_nth(c, i-1)->u.s, l);
       else
       add_variable(newenv, node_nth(c, i)->u.s, l);
       free_node(l);
       break;
       }
       */
    nn = eval_node(env, node);
    if (nn->t == NODE_ERROR) {
      free_env(newenv);
      free_node(f);
      return nn;
    }
    add_variable(newenv, c->u.s, nn);
    free_node(nn);
    node = node->cdr;
    c = c->cdr;
  }
  c = NULL;
  x = x->cdr->cdr->cdr;
  while (x) {
    if (c) free_node(c);
    c = eval_node(newenv, x);
    if (c->t == NODE_ERROR) break;
    x = x->cdr;
  }
  free_env(newenv);
  free_node(f);
  if (c) {
    return c;
  }
  return new_node();
}

static NODE*
do_lambda(ENV *env, NODE *node) {
  NODE *x;
  int i;

  if (node_narg(node) != 2) return new_errorf("malformed lambda: %s", node);

  x = new_node();
  x->t = NODE_LAMBDA;
  node_append(x, new_node());
  node_append(x, new_node());
  for (i = 1; i < node->n; i++) {
    node_nth(node, i)->r++;
    node_append(x, node_nth(node, i));
  }
  return x;
}

static NODE*
do_funcall(ENV *env, NODE *node) {
  NODE *x, *c, *nn;
  int i;

  if (node_narg(node) < 3) return new_errorf("malformed funcall: %s", node);

  x = do_ident(env, node_nth(node, 1));
  if (x->t == NODE_ERROR) return x;
  c = new_node();
  c->t = NODE_CELL;
  c->n = node->n - 1;
  node_append(c, x);
  for (i = 2; i < node->n; i++) {
    node_nth(node, i)->r++;
    node_append(c, node_nth(node, i));
  }
  nn = do_call(env, c);
  free_node(c);
  return nn;
}

static NODE*
do_defun(ENV *env, NODE *node) {
  NODE *x;

  if (node_narg(node) < 3) return new_errorn("malformed defun: %s", node);

  x = node->cdr;
  if (x->t != NODE_IDENT) {
    return new_errorn("invalid identifier: %s", x);
  }
  add_function(env, x->u.s, node);
  x->r++;
  return x;
}

static NODE*
do_progn(ENV *env, NODE *node) {
  NODE *c;
  int i;

  if (node_narg(node) < 1) return new_errorn("malformed progn: %s", node);

  c = NULL;
  for (i = 1; i < node->n; i++) {
    if (c) free_node(c);
    c = eval_node(env, node_nth(node, i));
    if (c->t == NODE_ERROR) break;
  }
  if (c) {
    return c;
  }
  return new_node();
}

static NODE*
do_dotimes(ENV *env, NODE *node) {
  ENV *newenv;
  NODE *x, *c, *nn, *err = NULL;
  int i, r;

  if (node_narg(node) != 2) return new_errorn("malformed dotimes: %s", node);

  x = node->cdr;
  c = eval_node(env, x->car->cdr);
  if (c->t == NODE_ERROR) return c;
  r = int_value(env, c, &err);
  free_node(c);
  if (err) return err;
  newenv = new_env(env);
  nn = new_node();
  nn->t = NODE_INT;
  add_variable(newenv, x->car->u.s, nn);
  c = NULL;
  for (i = 0; i < r; i++) {
    nn->u.i = i;
    if (c) free_node(c);
    c = eval_node(newenv, x->cdr);
    if (c->t == NODE_ERROR) break;
  }
  free_env(newenv);
  free_node(nn);
  if (c) {
    return c;
  }
  return new_node();
}

static NODE*
do_type_of(ENV *env, NODE *node) {
  NODE *c;
  const char *p = "unknown";

  if (!node->cdr) return new_errorn("malformed type-of: %s", node);

  if (node->cdr->t == NODE_QUOTE) {
    p = "symbol";
  }
  else {
    c = eval_node(env, node->cdr);
    if (c->t == NODE_ERROR) return c;
    switch (c->t) {
    case NODE_NIL: p = "null"; break;
    case NODE_T: p = "boolean"; break;
    case NODE_INT: p = "int"; break;
    case NODE_DOUBLE: p = "float"; break;
    case NODE_STRING: p = "string"; break;
    case NODE_QUOTE: p = "symbol"; break;
    case NODE_ERROR: p = "error"; break;
    }
    free_node(c);
  }
  c = new_node();
  c->t = NODE_STRING;
  c->u.s = strdup(p);
  return c;
}

static NODE*
do_cond(ENV *env, NODE *node) {
  NODE *x, *c, *err = NULL;
  int i, r;

  if (node_narg(node) < 1) return new_errorn("malformed cond: %s", node);

  c = NULL;
  for (i = 0; i < node->n; i++) {
    x = node_nth(node, i);
    if (x->t == NODE_QUOTE)
      x = node_nth(x, 0);
    if (c) free_node(c);
    c = eval_node(env, node_nth(x, 0));
    if (c->t == NODE_ERROR) break;
    r = int_value(env, c, &err);
    if (err) {
      free_node(c);
      return err;
    }
    if (r != 0) {
      free_node(c);
      return eval_node(env, node_nth(x, 1));
    }
  }
  if (c) {
    return c;
  }
  return new_node();
}

static NODE*
do_car(ENV *env, NODE *node) {
  NODE *x, *c;

  if (!node->cdr) return new_errorn("malformed car: %s", node);

  x = eval_node(env, node->cdr);
  if (x->t != NODE_CELL && x->t != NODE_NIL) {
    free_node(x);
    return new_errorn("argument is not a list: %s", node);
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
do_cdr(ENV *env, NODE *node) {
  NODE *x, *c;

  if (!node->cdr) return new_errorn("malformed cdr: %s", node);

  x = eval_node(env, node->cdr);
  if (x->t != NODE_CELL && x->t != NODE_NIL) {
    free_node(x);
    return new_errorn("argument is not a list: %s", node);
  }
  if (x->cdr) {
    c = x->cdr;
    c->r++;
    free_node(x);
    return c;
  }
  if (x->car) {
    c = x->car;
    c->t = NODE_CELL;
    c->r++;
    free_node(x);
    return c;
  }
  free_node(x);
  return new_node();
}

static NODE*
do_rplaca(ENV *env, NODE *node) {
  NODE *lhs, *rhs;

  if (node_narg(node) != 2) return new_errorn("malformed rplaca: %s", node);

  lhs = eval_node(env, node_nth(node, 1));
  if (lhs->t == NODE_ERROR) return lhs;
  if ((lhs->t != NODE_CELL) || lhs->n == 0)
    return new_errorn("malformed rplaca: %s", node);
  rhs = eval_node(env, node_nth(node, 2));
  if (rhs->t == NODE_ERROR) {
    free_node(lhs);
    return rhs;
  }
  free_node(node_nth(lhs, 0));
  *(node_nth(lhs, 0)) = *(rhs);
  free_node(rhs);
  return lhs;
}

static NODE*
do_rplacd(ENV *env, NODE *node) {
  NODE *lhs, *rhs;

  if (node_narg(node) != 3) return new_errorn("malformed rplacd: %s", node);

  lhs = eval_node(env, node_nth(node, 1));
  if (lhs->t == NODE_ERROR) return lhs;
  if ((lhs->t != NODE_CELL) || lhs->n == 0)
    return new_errorn("malformed rplaca: %s", node);
  rhs = eval_node(env, node_nth(node, 2));
  if (rhs->t == NODE_ERROR) {
    free_node(lhs);
    return rhs;
  }
  free_node(node_nth(lhs, 1));
  lhs->t = NODE_CELL;
  *(node_nth(lhs, 1)) = *(rhs);
  free_node(rhs);
  return lhs;
}

static NODE*
do_cons(ENV *env, NODE *node) {
  int i;
  NODE *c, *lhs, *rhs;

  if (node_narg(node) != 2) return new_errorn("malformed cons: %s", node);

  lhs = eval_node(env, node->cdr);
  if (lhs->t == NODE_ERROR) return lhs;
  rhs = eval_node(env, node->cdr->cdr);
  if (rhs->t == NODE_ERROR) {
    free_node(lhs);
    return rhs;
  }
  c = new_node();
  switch (rhs->t) {
  case NODE_NIL:
  case NODE_CELL:
      c->t = rhs->t;
      c->n = rhs->n + 1;
      c->cdr = lhs;
      for (i = 1; i < c->n; i++) {
        node_nth(rhs, i - 1)->r++;
        node_append(c, node_nth(rhs, i - 1));
      }
      free_node(rhs);
      break;
  default:
      c->t = NODE_CELL;
      c->cdr = lhs;
      c->cdr = rhs;
      break;
  }
  return c;
}

static NODE*
do_length(ENV *env, NODE *node) {
  NODE *x, *c;

  if (!node->cdr) return new_errorn("malformed length: %s", node);

  x = eval_node(env, node->cdr);
  if (x->t != NODE_CELL && x->t != NODE_NIL && x->t != NODE_STRING) {
    free_node(x);
    return new_errorn("argument is not a list: %s", node);
  }
  c = new_node();
  c->t = NODE_INT;
  c->u.i = x->t == NODE_STRING ? strlen(x->u.s) : node_narg(x);
  free_node(x);
  return c;
}

static NODE*
do_concatenate(ENV *env, NODE *node) {
  NODE *x, *c, *l, *nn;
  int i;

  if (node_narg(node) < 3) return new_errorn("malformed concatenate: %s", node);

  x = eval_node(env, node_nth(node, 1));
  if (x->t != NODE_IDENT) {
    free_node(x);
    return new_errorn("first argument is not a quote: %s", node);
  }
  l = eval_node(env, node_nth(node, 2));
  if (l->t != NODE_CELL && l->t != NODE_NIL && l->t != NODE_STRING) {
    free_node(x);
    return new_errorn("argument is not a list: %s", node);
  }
  c = new_node();
  c->t = !strcmp(x->u.s, "string") ? NODE_STRING : NODE_CELL;
  for (i = 2; i < node->n; i++) {
    if (c->t == NODE_STRING) {
      nn = eval_node(env, node_nth(node, i));
      if (nn->t == NODE_ERROR) {
        free_node(c);
        c = nn;
        break;
      }
      if (nn->t != NODE_STRING) {
        free_node(c);
        c = new_errorn("argument is not string: %s", nn);
        free_node(nn);
        break;
      }
      /* TODO: slow growing-up */
      if (c->u.s) {
        c->u.s = (char*)realloc(c->u.s, strlen(c->u.s) + strlen(nn->u.s) + 1);
        strcat(c->u.s, nn->u.s);
      }
      else {
        c->u.s = (char*)malloc(strlen(nn->u.s) + 1);
        strcpy(c->u.s, nn->u.s);
      }
      free_node(nn);
    }
  }
  free_node(x);
  free_node(l);
  return c;
}

static NODE*
do_make_string(ENV *env, NODE *node) {
  NODE *x, *c;

  if (node_narg(node) != 1) return new_errorn("malformed make-string: %s", node);

  x = eval_node(env, node->cdr);
  if (x->t == NODE_ERROR) return x;
  if (x->t != NODE_INT || x->u.i < 0) {
    free_node(x);
    return new_errorn("malformed make-string: %s", node);
  }
  c = new_node();
  c->t = NODE_STRING;
  c->u.s = (char*)malloc(x->u.i + 1);
  memset(c->u.s, ' ', x->u.i);
  *(c->u.s + x->u.i) = 0;
  free_node(x);
  return c;
}

static NODE*
do_load(ENV *env, NODE *node) {
  NODE *x, *ret, *top;
  char *p, *t;
  long fsize;

  if (node->n != 1) return new_errorn("malformed load: %s", node);
  x = eval_node(env, node_nth(node, 0));
  if (x->t == NODE_ERROR) return x;
  if (x->t != NODE_STRING) {
    free_node(x);
    return new_errorn("malformed load: %s", node);
  }

  FILE *fp = fopen(node_nth(node, 0)->u.s, "rb");
  if (!fp) {
    fclose(fp);
    return new_errorf("%s", strerror(errno));
  }
  fseek(fp, 0, SEEK_END);
  fsize = ftell(fp);
  fseek(fp, 0, SEEK_SET);
  t = p = (char*)malloc(fsize + 1);
  memset(p, 0, fsize + 1);
  if (!fread(p, fsize, 1, fp)) {
    free((void*)t);
    fclose(fp);
    return new_errorf("%s", strerror(errno));
  }
  fclose(fp);

  top = new_node();
  top->t = NODE_CELL;
  p = (char*)parse_paren(top, p);
  if (!p) {
    free_node(top);
    free_node(x);
    return new_errorn("failed to load: %s", node);
  }
  p = (char*)skip_white((char*)p);
  if (*p) {
    free_node(top);
    return new_errorf("invalid token: %s", node);
  }
  free((char*)t);
  ret = eval_node(env, top);
  if (ret->t == NODE_ERROR) return ret;
  free_node(ret);
  free_node(top);
  free_env(env);
  ret = new_node();
  ret->t = NODE_T;
  return ret;
}

static NODE*
do_apply(ENV *env, NODE *node) {
  NODE *x, *a, *c, *nn;
  int i;

  if (node_narg(node) < 3) return new_errorn("malformed apply: %s", node);

  a = eval_node(env, node_nth(node, 1));
  if (a->t != NODE_LAMBDA && a->t != NODE_IDENT) {
    free_node(a);
    return new_errorn("first argument should be function: %s", node);
  }
  x = eval_node(env, node_nth(node, 2));
  if (x->t != NODE_CELL) {
    free_node(a);
    free_node(x);
    return new_errorn("second argument should be list: %s", node);
  }
  c = new_node();
  c->t = NODE_CELL;
  c->n = 3;
  node_append(c, a);
  node_nth(x, 0)->r++;
  node_append(c, node_nth(x, 0));
  for (i = 1; i < x->n; i++) {
    node_set_nth(c, 2, node_nth(x, i));
    if (a->t == NODE_LAMBDA)
      nn = do_call(env, c);
    else
      nn = eval_node(env, c);
    free_node(node_nth(c, 1));
    if (nn->t == NODE_ERROR) {
      free_node(c);
      return nn;
    }
    node_set_nth(c, 1, nn);
  }

  free_node(x);
  x = node_nth(c, 1);

  c->n = 0;
  c->cdr = NULL;
  free_node(c);
  free_node(a);
  x->r++;
  return x;
}

static void
add_sym(ENV *env, enum T t, const char* n, f_do f) {
  ITEM *ni;
  NODE *node;
  node = new_node();
  node->t = t;
  node->u.s = strdup(n);
  node->f = f;
  ni = (ITEM*)malloc(sizeof(ITEM));
  memset(ni, 0, sizeof(ITEM));
  ni->k = strdup(n);
  ni->v = node;
  env->lv = (ITEM**)realloc(env->lv, sizeof(ITEM*) * (env->nv + 1));
  env->lv[env->nv] = ni;
  env->nv++;
}

static void
add_defaults(ENV *env) {
  add_sym(env, NODE_IDENT, "+", do_plus);
  add_sym(env, NODE_IDENT, "-", do_minus);
  add_sym(env, NODE_IDENT, "*", do_mul);
  add_sym(env, NODE_IDENT, "/", do_div);
  add_sym(env, NODE_IDENT, "1+", do_plus1);
  add_sym(env, NODE_IDENT, "1-", do_minus1);
  add_sym(env, NODE_IDENT, "not", do_not);
  add_sym(env, NODE_IDENT, "mod", do_mod);
  add_sym(env, NODE_IDENT, "%", do_mod);
  add_sym(env, NODE_IDENT, "if", do_if);
  add_sym(env, NODE_IDENT, ">", do_gt);
  add_sym(env, NODE_IDENT, ">=", do_ge);
  add_sym(env, NODE_IDENT, "<", do_lt);
  add_sym(env, NODE_IDENT, "<=", do_le);
  add_sym(env, NODE_IDENT, "=", do_eq);
  add_sym(env, NODE_IDENT, "eq", do_eq);
  add_sym(env, NODE_IDENT, "print", do_print);
  add_sym(env, NODE_IDENT, "println", do_println);
  add_sym(env, NODE_IDENT, "princ", do_princ);
  add_sym(env, NODE_IDENT, "quote", do_quote);
  add_sym(env, NODE_IDENT, "setq", do_setq);
  add_sym(env, NODE_IDENT, "let", do_let);
  add_sym(env, NODE_IDENT, "defun", do_defun);
  add_sym(env, NODE_IDENT, "progn", do_progn);
  add_sym(env, NODE_IDENT, "cond", do_cond);
  add_sym(env, NODE_IDENT, "car", do_car);
  add_sym(env, NODE_IDENT, "cdr", do_cdr);
  add_sym(env, NODE_IDENT, "rplaca", do_rplaca);
  add_sym(env, NODE_IDENT, "rplacd", do_rplacd);
  add_sym(env, NODE_IDENT, "length", do_length);
  add_sym(env, NODE_IDENT, "concatenate", do_concatenate);
  add_sym(env, NODE_IDENT, "cons", do_cons);
  add_sym(env, NODE_IDENT, "apply", do_apply);
  add_sym(env, NODE_IDENT, "dotimes", do_dotimes);
  add_sym(env, NODE_IDENT, "lambda", do_lambda);
  add_sym(env, NODE_IDENT, "funcall", do_funcall);
  add_sym(env, NODE_IDENT, "type-of", do_type_of);
  add_sym(env, NODE_IDENT, "load", do_load);
  add_sym(env, NODE_IDENT, "make-string", do_make_string);
  add_sym(env, NODE_NIL, "nil", NULL);
  add_sym(env, NODE_T, "t", NULL);
}

static NODE*
eval_node(ENV *env, NODE *node) {
  NODE *c = NULL;
  switch (node->t) {
  case NODE_QUOTE:
      c = node->car;
      c->r++;
      return c;
  case NODE_IDENT:
      return do_call(env, node);
  case NODE_LAMBDA:
      node->r++;
      return node;
  case NODE_INT:
      node->r++;
      return node;
  case NODE_DOUBLE:
      node->r++;
      return node;
  case NODE_NIL:
      node->r++;
      return node;
  case NODE_T:
      node->r++;
      return node;
  case NODE_STRING:
      node->r++;
      return node;
  case NODE_CELL:
      c = NULL;
      if (node->car) c = eval_node(env, node->car);
      if (node->cdr) {
        if (c) free_node(c);
        c = eval_node(env, node->cdr);
      }
      return c;
  }

  return new_error("unknown node");
}

#if 0
static NODE*
run_node(ENV *env, NODE *node) {
  NODE *c = NULL;
  int i;

  for (i = 0; i < node->n; i++) {
    if (c) free_node(c);
    c = eval_node(env, node_nth(node, i));
    if (c->t == NODE_ERROR) break;
  }
  if (c) {
    return c;
  }
  return new_node();
}
#endif

int
main(int argc, char* argv[]) {
  ENV *env;
  NODE *top, *ret;
  char buf[BUFSIZ], *p;
  const char *pp;
  long fsize;

  if (argc > 1) {
    FILE *fp = fopen(argv[1], "rb");

    if (!fp) {
      fprintf(stderr, "%s: %s\n", argv[0], strerror(errno));
      exit(1);
    }
    fseek(fp, 0, SEEK_END);
    fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    p = (char*)malloc(fsize + 1);
    pp = p;
    memset(p, 0, fsize + 1);
    if (!fread(p, fsize, 1, fp)) {
      fprintf(stderr, "%s: %s\n", argv[0], strerror(errno));
      exit(1);
    }
    fclose(fp);

    env = new_env(NULL);
    add_defaults(env);
    top = new_node();
    top->t = NODE_CELL;
    p = (char*)parse_paren(top, p);
    if (!p) {
      free_node(top);
      free_env(env);
      exit(1);
    }
    p = (char*)skip_white((char*)p);
    if (*p) {
      free_node(top);
      free_env(env);
      raise(p);
      exit(1);
    }
    free((char*)pp);
    ret = eval_node(env, top);
    if (ret->t == NODE_ERROR)
      fprintf(stderr, "%s: %s\n", argv[0], ret->u.s);
    free_node(ret);
    free_node(top);
    free_env(env);
    exit(0);
  }

  env = new_env(NULL);
  add_defaults(env);
  while (1) {
    if (isatty(fileno(stdin))) {
      printf("> ");
      if (!fgets(buf, sizeof(buf), stdin)) break;
    }
    else {
      fsize = fread(buf, 1, sizeof(buf), stdin);
      if (fsize <= 0) break;
      buf[fsize] = 0;
    }
    top = new_node();
    top->t = NODE_CELL;
    pp = parse_any(top, buf);
    if (!pp) {
      continue;
    }
    pp = skip_white(pp);
    if (*pp) {
      raise(pp);
      continue;
    }
    ret = eval_node(env, top);
    if (ret->t == NODE_ERROR) {
      fprintf(stderr, "%s: %s\n", argv[0], ret->u.s);
    }
    else if (isatty(fileno(stdin))) {
      buf[0] = 0;
      print_node(sizeof(buf), buf, ret, 0);
      puts(buf);
    }
    free_node(ret);
    free_node(top);
  }
  free_env(env);
  return 0;
}

/* vim:set et cino=>2,\:0: */
