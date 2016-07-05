#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <memory.h>
#include <unistd.h>
#include <ctype.h>

enum T {
  NODE_NIL, NODE_T, NODE_INT, NODE_DOUBLE, NODE_STRING, NODE_QUOTE, NODE_IDENT, NODE_LIST,
  NODE_CALL, NODE_PROGN, NODE_ERROR,
};

typedef struct _NODE {
  int t;
  union {
    long i;
    double d;
    char* s;
  } u;
  void *f;
  int n;
  int r;
  struct _NODE **c;
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

static const char* parse_any(ENV *env, NODE *node, const char *p);
static const char* parse_paren(ENV *env, NODE *node, const char *p);
static const char* parse_ident(ENV *env, NODE *node, const char *p);
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
    } else if (isspace((int)*p)) p++;
    else break;
  }
  return p;
}

static NODE*
new_node() {
  NODE* node = (NODE*) malloc(sizeof(NODE));
  memset(node, 0, sizeof(NODE));
  node->t = NODE_NIL;
  node->r++;
  return node;
}

static NODE*
new_error(const char* msg) {
  NODE* node = (NODE*) malloc(sizeof(NODE));
  memset(node, 0, sizeof(NODE));
  node->t = NODE_ERROR;
  node->u.s = strdup(msg);
  return node;
}

static NODE*
new_errorf(const char* fmt, ...) {
  char buf[BUFSIZ];
  va_list list;
  va_start(list, fmt);
  vsnprintf(buf, sizeof(buf), fmt, list);
  va_end(list);
  NODE* node = (NODE*) malloc(sizeof(NODE));
  memset(node, 0, sizeof(NODE));
  node->t = NODE_ERROR;
  node->u.s = strdup(buf);
  return node;
}

static ENV*
new_env(ENV *p) {
  ENV* env = (ENV*) malloc(sizeof(ENV));
  memset(env, 0, sizeof(ENV));
  env->p = p;
  return env;
}

static int
match(const char *lhs, const char *rhs, size_t n) {
  const char *p = lhs, *e = lhs + n;
  while (p < e) if (*p++ != *rhs++) return 0;
  if (*p || *rhs) return 0;
  return 1;
}

static const char*
parse_number(ENV *env, NODE *node, const char* p) {
  const char *t = p;
  if (*p == '-') p++;
  while (*p && isdigit(*p)) p++;
  if (*p != '.') {
    node->t = NODE_INT;
    node->u.i = atoi(t);
  } else if (*p) {
    p++;
    if (!isdigit(*p)) {
      return parse_ident(env, node, t);
    }
    while (*p && isdigit(*p)) p++;
    node->t = NODE_DOUBLE;
    node->u.d = atof(t);
  }
  return p;
}

static const char*
parse_args(ENV *env, NODE *node, const char *p) {
  if (!p) return NULL;
  p = skip_white(p);
  while (p && *p && *p != ')') {
    NODE *child = new_node();
    p = parse_any(env, child, p);
    if (!p) {
      free_node(child);
      return NULL;
    }
    node->c = (NODE**) realloc(node->c, sizeof(NODE*) * (node->n + 1));
    node->c[node->n] = child;
    node->n++;
    p = skip_white(p);
  }
  if (p && *p) {
    if (*p == ')') p++;
    else return raisef("shoud be )", p);
  }
  return p;
}

static const char*
parse_paren(ENV *env, NODE *node, const char *p) {
  if (!p) return NULL;
  const char *t = p;
  while (!isspace(*p) && *p != ')') p++;
  p = parse_ident(env, node, t);
  node->t = NODE_CALL;
  p = parse_args(env, node, p);
  return p;
}

static const char*
parse_ident(ENV *env, NODE *node, const char *p) {
  const char *t = p;
  while (*p && (isalpha(*p) || isdigit(*p) || strchr("+-*/<>=", *p))) p++;
  if (match(t, "nil", (size_t)(p - t))) {
    node->t = NODE_NIL;
    return p;
  }
  if (match(t, "t", (size_t)(p - t))) {
    node->t = NODE_T;
    return p;
  }
  node->t = NODE_IDENT;
  node->u.s = (char*) malloc((size_t)(p - t) + 1);
  memset(node->u.s, 0, (size_t)(p - t) + 1);
  memcpy(node->u.s, t, (size_t)(p - t));
  return p;
}

static const char*
parse_quote(ENV *env, NODE *node, const char *p) {
  NODE *child = new_node();
  if (*p == '(') {
    child->t = NODE_LIST;
    p = parse_args(env, child, p+1);
  } else {
    p = parse_ident(env, child, p);
  }
  if (!p) {
    free_node(child);
    return NULL;
  }
  node->t = NODE_QUOTE;
  node->c = (NODE**) realloc(node->c, sizeof(NODE*) * (node->n + 1));
  node->c[node->n] = child;
  node->n++;
  return p;
}

static const char*
parse_string(ENV *env, NODE *node, const char *p) {
  const char *t = p;
  char *sp;
  int n = 0;
  while (*p) {
    if (*p == '\\' && *(p + 1)) p++;
    else if (*p == '"') break;
    p++;
    n++;
  }
  node->u.s = (char*) malloc(n + 1);
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
parse_any(ENV *env, NODE *node, const char *p) {
  if (!p) return NULL;
  p = skip_white(p);
  if (*p == '(') {
    return parse_paren(env, node, p + 1);
  }
  if (*p == '-' || isdigit(*p)) return parse_number(env, node, p);
  if (*p == '\'') return parse_quote(env, node, p + 1);
  if (*p == '"') return parse_string(env, node, p + 1);
  if (isalpha(*p) || strchr("+-*/<>=", *p)) return parse_ident(env, node, p);
  if (*p) return raise(p);
  return p;
}

static void
print_args(size_t nbuf, char *buf, NODE *node, int mode) {
  int i;
  for (i = 0; i < node->n; i++) {
    strncat(buf, " ", nbuf);
    print_node(nbuf, buf, node->c[i], mode);
  }
}

static void
print_list(size_t nbuf, char *buf, NODE *node, int mode) {
  int i;
  for (i = 0; i < node->n; i++) {
    if (i > 0) strncat(buf, " ", nbuf);
    print_node(nbuf, buf, node->c[i], mode);
  }
}

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
print_node(size_t nbuf, char* buf, NODE *node, int mode) {
  char tmp[BUFSIZ];
  switch (node->t) {
  case NODE_INT: snprintf(tmp, sizeof(tmp), "%ld", node->u.i); strncat(buf, tmp, nbuf); break;
  case NODE_DOUBLE: snprintf(tmp, sizeof(tmp), "%f", node->u.d); strncat(buf, tmp, nbuf); break;
  case NODE_STRING: print_str(nbuf, buf, node, mode); break;
  case NODE_IDENT: snprintf(tmp, sizeof(tmp), "%s", node->u.s); strncat(buf, tmp, nbuf); break;
  case NODE_NIL: strncat(buf, "nil", nbuf); break;
  case NODE_T: strncat(buf, "t", nbuf); break;
  case NODE_QUOTE: strncat(buf, "'", nbuf); print_node(nbuf, buf, node->c[0], mode); break;
  case NODE_LIST: strncat(buf, "(", nbuf); print_list(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_PROGN: strncat(buf, "(progn", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_CALL: snprintf(tmp, sizeof(tmp), "(%s", node->u.s); strncat(buf, tmp, nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  }
}

static void
free_node(NODE *node) {
  int i;
  node->r--;
  if (node->r <= 0) {
    for (i = 0; i < node->n; i++)
      free_node(node->c[i]);
    free(node->c);
    switch (node->t) {
    case NODE_STRING:
    case NODE_IDENT:
    case NODE_CALL:
    case NODE_ERROR:
    case NODE_NIL:
    case NODE_T:
      free(node->u.s);
      break;
    }
    free(node);
  }
}

static void
free_env(ENV *env) {
  int i;
  for (i = 0; i < env->nv; i++) {
    free((void*)env->lv[i]->k);
    free_node(env->lv[i]->v);
    free(env->lv[i]);
  }
  for (i = 0; i < env->nf; i++) {
    free((void*)env->lf[i]->k);
    free_node(env->lf[i]->v);
    free(env->lf[i]);
  }
  free(env->lv);
  free(env);
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
  ni = (ITEM*) malloc(sizeof(ITEM));
  memset(ni, 0, sizeof(ITEM));
  ni->k = strdup(k);
  ni->v = node;
  env->lv = (ITEM**) realloc(env->lv, sizeof(ITEM*) * (env->nv + 1));
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
  ni = (ITEM*) malloc(sizeof(ITEM));
  memset(ni, 0, sizeof(ITEM));
  ni->k = strdup(k);
  ni->v = node;
  env->lf = (ITEM**) realloc(env->lf, sizeof(ITEM*) * (env->nf + 1));
  env->lf[env->nf] = ni;
  env->nf++;
}

static long
int_value(ENV *env, NODE *node) {
  node = eval_node(env, node);
  int r = 0;
  switch (node->t) {
  case NODE_NIL: r = 0; break;
  case NODE_T: r = 1; break;
  case NODE_INT: r = node->u.i; break;
  case NODE_DOUBLE: r = (long)node->u.d; break;
  case NODE_QUOTE: r = int_value(env, node->c[0]); break;
  }
  free_node(node);
  return r;
}

static double
double_value(ENV *env, NODE *node) {
  node = eval_node(env, node);
  double r = 0;
  switch (node->t) {
  case NODE_INT: r = (double)node->u.i; break;
  case NODE_DOUBLE: r = node->u.d; break;
  case NODE_QUOTE: r = double_value(env, node->c[0]); break;
  }
  free_node(node);
  return r;
}

static NODE*
do_plus(ENV *env, NODE *node) {
  NODE *nn, *c;
  int i;

  if (node->n < 2) return new_errorf("malformed +");
  nn = new_node();
  for (i = 0; i < node->n; i++) {
    c = eval_node(env, node->c[i]);
    if (i == 0) {
      nn->t = c->t;
      nn->u = c->u;
      free_node(c);
      continue;
    }
    switch (nn->t) {
      case NODE_INT:
        if (c->t == NODE_DOUBLE) {
          nn->u.d = double_value(env, nn) + double_value(env, c);
          nn->t = c->t;
        } else
          nn->u.i += int_value(env, c);
        break;
      case NODE_DOUBLE: nn->u.d += double_value(env, c); break;
      default: break;
    }
    free_node(c);
  }
  return nn;
}

static NODE*
do_minus(ENV *env, NODE *node) {
  NODE *nn, *c;
  int i;

  if (node->n < 2) return new_errorf("malformed -");
  nn = new_node();
  for (i = 0; i < node->n; i++) {
    c = eval_node(env, node->c[i]);
    if (i == 0) {
      nn->t = c->t;
      nn->u = c->u;
      free_node(c);
      continue;
    }
    switch (nn->t) {
      case NODE_INT:
        if (c->t == NODE_DOUBLE) {
          nn->u.d = double_value(env, nn) - double_value(env, c);
          nn->t = c->t;
        } else
          nn->u.i -= int_value(env, c);
        break;
      case NODE_DOUBLE: nn->u.d -= double_value(env, c); break;
      default: break;
    }
    free_node(c);
  }
  return nn;
}

static NODE*
do_mul(ENV *env, NODE *node) {
  NODE *nn, *c;
  int i;

  if (node->n < 2) return new_errorf("malformed *");
  nn = new_node();
  for (i = 0; i < node->n; i++) {
    c = eval_node(env, node->c[i]);
    if (i == 0) {
      nn->u = c->u;
      nn->t = c->t;
      free_node(c);
      continue;
    }
    switch (nn->t) {
      case NODE_INT:
        if (c->t == NODE_DOUBLE) {
          nn->u.d = double_value(env, nn) * double_value(env, c);
          nn->t = c->t;
        } else
          nn->u.i *= int_value(env, c);
        break;
      case NODE_DOUBLE: nn->u.d *= double_value(env, c); break;
      default: break;
    }
    free_node(c);
  }
  return nn;
}

static NODE*
do_div(ENV *env, NODE *node) {
  NODE *nn, *c;
  int i;

  if (node->n < 2) return new_errorf("malformed /");
  nn = new_node();
  for (i = 0; i < node->n; i++) {
    c = eval_node(env, node->c[i]);
    if (i == 0) {
      nn->t = c->t;
      nn->u = c->u;
      free_node(c);
      continue;
    }
    switch (nn->t) {
      case NODE_INT:
        if (c->t == NODE_DOUBLE) {
          nn->u.d = double_value(env, nn) / double_value(env, c);
          nn->t = c->t;
        } else
          nn->u.i /= int_value(env, c);
        break;
      case NODE_DOUBLE: nn->u.d /= double_value(env, c); break;
      default: break;
    }
    free_node(c);
  }
  return nn;
}

static NODE*
do_plus1(ENV *env, NODE *node) {
  NODE *x, *c;

  if (node->n != 1) return new_errorf("malformed 1+");
  c = new_node();
  x = eval_node(env, node->c[0]);
  c->t = x->t;
  switch (c->t) {
    case NODE_INT: c->u.i = x->u.i + 1; break;
    case NODE_DOUBLE: c->u.d = x->u.i + 1.0; break;
    default: break;
  }
  free_node(x);
  return c;
}

static NODE*
do_minus1(ENV *env, NODE *node) {
  NODE *x, *c;

  if (node->n != 1) return new_errorf("malformed 1-");
  c = new_node();
  x = eval_node(env, node->c[0]);
  c->t = x->t;
  switch (c->t) {
    case NODE_INT: c->u.i = x->u.i - 1; break;
    case NODE_DOUBLE: c->u.d = x->u.i - 1.0; break;
    default: break;
  }
  free_node(x);
  return c;
}

static NODE*
do_not(ENV *env, NODE *node) {
  NODE *c;

  if (node->n != 1) return new_errorf("malformed not");
  c = new_node();
  c->t = NODE_INT;
  c->u.i = !int_value(env, node->c[0]);
  return c;
}

static NODE*
do_mod(ENV *env, NODE *node) {
  NODE *c;

  if (node->n != 2) return new_errorf("malformed not");
  c = new_node();
  c->t = NODE_INT;
  c->u.i = int_value(env, node->c[0]) % int_value(env, node->c[1]);
  return c;
}

static NODE*
do_if(ENV *env, NODE *node) {
  NODE *x, *c;
  int r = 0;

  if (node->n != 3) return new_errorf("malformed if");
  c = eval_node(env, node->c[0]);
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
      r = (long) c->u.d;
      break;
    default:
      r = 1;
      break;
  }
  free_node(c);
  x = eval_node(env, node->c[r > 0 ? 1 : 2]);
  return x;
}

static NODE*
do_gt(ENV *env, NODE *node) {
  NODE *nn;

  if (node->n != 2) return new_errorf("malformed >");
  nn = new_node();
  if (double_value(env, node->c[0]) > double_value(env, node->c[1])) {
    nn->t = NODE_T;
  }
  return nn;
}

static NODE*
do_ge(ENV *env, NODE *node) {
  NODE *nn;

  if (node->n != 2) return new_errorf("malformed >=");
  nn = new_node();
  if (double_value(env, node->c[0]) >= double_value(env, node->c[1])) {
    nn->t = NODE_T;
  }
  return nn;
}

static NODE*
do_lt(ENV *env, NODE *node) {
  NODE *nn;

  if (node->n != 2) return new_errorf("malformed <");
  nn = new_node();
  if (double_value(env, node->c[0]) < double_value(env, node->c[1])) {
    nn->t = NODE_T;
  }
  return nn;
}

static NODE*
do_le(ENV *env, NODE *node) {
  NODE *nn;

  if (node->n != 2) return new_errorf("malformed <=");
  nn = new_node();
  if (double_value(env, node->c[0]) <= double_value(env, node->c[1])) {
    nn->t = NODE_T;
  }
  return nn;
}

static NODE*
do_eq(ENV *env, NODE *node) {
  NODE *nn;

  if (node->n != 2) return new_errorf("malformed =");
  nn = new_node();
  /* TODO: string comparison */
  if (int_value(env, node->c[0]) == int_value(env, node->c[1])) {
    nn->t = NODE_T;
  }
  return nn;
}

static NODE*
do_print(ENV *env, NODE *node) {
  NODE *c;
  char buf[BUFSIZ];

  if (node->n != 1) return new_errorf("malformed print");
  c = eval_node(env, node->c[0]);
  buf[0] = 0;
  print_node(sizeof(buf), buf, c, 0);
  puts(buf);
  return c;
}

static NODE*
do_println(ENV *env, NODE *node) {
  NODE *c;
  char buf[BUFSIZ];

  if (node->n != 1) return new_errorf("malformed println");
  c = eval_node(env, node->c[0]);
  buf[0] = 0;
  print_node(sizeof(buf), buf, c, 0);
  puts(buf);
  return c;
}

static NODE*
do_princ(ENV *env, NODE *node) {
  NODE *c;
  char buf[BUFSIZ];

  if (node->n != 1) return new_errorf("malformed printc");
  c = eval_node(env, node->c[0]);
  buf[0] = 0;
  print_node(sizeof(buf), buf, c, 1);
  puts(buf);
  return c;
}

static NODE*
do_quote(ENV *env, NODE *node) {
  NODE *c;

  if (node->n != 1) return new_errorf("malformed quote");
  c = node->c[0];
  c->r++;
  return c;
}

static NODE*
do_setq(ENV *env, NODE *node) {
  NODE *x;
  char buf[BUFSIZ];

  if (node->n != 2) return new_errorf("malformed setq");
  x = node->c[0];
  if (x->t != NODE_IDENT) {
    buf[0] = 0;
    print_node(sizeof(buf), buf, x, 0);
    return new_errorf("invalid identifier: %s", buf);
  }
  add_variable(env, x->u.s, node->c[1]);
  node->c[1]->r++;
  return node->c[1];
}

static NODE*
do_ident(ENV *env, NODE *node) {
  NODE *x;
  int i;

  if (node->n != 0) return new_errorf("malformed ident");
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
  } else {
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

static char*
byname(NODE *node, int i) {
  return i == 0 ? node->u.s : node->c[i-1]->u.s;
}

static NODE*
do_call(ENV *env, NODE *node) {
  ENV *newenv;
  NODE *x, *c, *nn;
  int i;

  x = look_func(env, node->u.s);
  if (!x) {
    return new_errorf("unknown function: %s", node->u.s);
  }
  newenv = new_env(env);

  c = x->c[1];

  for (i = 0; i < node->n; i++) {
    nn = eval_node(env, node->c[i]);
    add_variable(newenv, byname(c, i), nn);
    free_node(nn);
  }
  c = NULL;
  for (i = 2; i < x->n; i++) {
    if (c) free_node(c);
    c = eval_node(newenv, x->c[i]);
  }
  free_env(newenv);
  free_node(x);
  if (c) {
    return c;
  }
  return new_node();
}

static NODE*
do_defun(ENV *env, NODE *node) {
  NODE *x;
  char buf[BUFSIZ];

  if (node->n < 3) return new_errorf("malformed defun");
  x = node->c[0];
  if (x->t != NODE_IDENT) {
    buf[0] = 0;
    print_node(sizeof(buf), buf, x, 0);
    return new_errorf("invalid identifier: %s", buf);
  }
  add_function(env, x->u.s, node);
  node->r++;
  return node;
}

static NODE*
do_progn(ENV *env, NODE *node) {
  NODE *c;
  int i;

  if (node->n < 1) return new_errorf("malformed progn");
  c = NULL;
  for (i = 0; i < node->n; i++) {
    if (c) free_node(c);
    c = eval_node(env, node->c[i]);
  }
  if (c) {
    return c;
  }
  return new_node();
}

static NODE*
do_dotimes(ENV *env, NODE *node) {
  ENV *newenv;
  NODE *x, *c, *nn;
  ITEM *ni;
  int i, r;

  if (node->n != 2) return new_errorf("malformed dotimes");
  x = node->c[0];
  c = eval_node(env, x->c[0]);
  r = int_value(env, c);
  free_node(c);
  newenv = new_env(env);
  ni = (ITEM*) malloc(sizeof(ITEM));
  memset(ni, 0, sizeof(ITEM));
  nn = new_node();
  nn->t = NODE_INT;
  add_variable(newenv, x->u.s, nn);
  c = NULL;
  for (i = 0; i < r; i++) {
    nn->u.i = i;
    if (c) free_node(c);
    c = eval_node(newenv, node->c[1]);
  }
  free_env(newenv);
  free_node(nn);
  if (c) {
    return c;
  }
  return new_node();
}

static NODE*
do_cond(ENV *env, NODE *node) {
  NODE *x, *c;
  int i, r;

  if (node->n < 1) return new_errorf("malformed cond");
  c = NULL;
  for (i = 0; i < node->n; i++) {
    x = node->c[i];
    if (x->t == NODE_QUOTE)
      x = x->c[0];
    if (c) free_node(c);
    c = eval_node(env, x->c[0]);
    r = int_value(env, c);
    if (r != 0) {
      free_node(c);
      return eval_node(env, x->c[1]);
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
  char buf[BUFSIZ];

  if (node->n != 1) return new_errorf("malformed car");
  x = node->c[0];
  if (x->t != NODE_QUOTE) {
    buf[0] = 0;
    print_node(sizeof(buf), buf, x, 0);
    return new_errorf("not quote: %s", buf);
  }
  x = x->c[0];
  if (x->t != NODE_LIST) {
    buf[0] = 0;
    print_node(sizeof(buf), buf, x, 0);
    return new_errorf("not list: %s", buf);
  }
  if (x->n > 0) {
    c = x->c[0];
    c->r++;
    return c;
  }
  return new_node();
}

static NODE*
do_cdr(ENV *env, NODE *node) {
  NODE *x, *c;
  int i;
  char buf[BUFSIZ];

  if (node->n != 1) return new_errorf("malformed cdr");
  x = node->c[0];
  if (x->t != NODE_QUOTE) {
    buf[0] = 0;
    print_node(sizeof(buf), buf, x, 0);
    return new_errorf("not quote: %s", buf);
  }
  x = x->c[0];
  if (x->t != NODE_LIST) {
    buf[0] = 0;
    print_node(sizeof(buf), buf, x, 0);
    return new_errorf("not list: %s", buf);
  }
  if (x->n > 0) {
    c = new_node();
    c->t = NODE_LIST;
    c->c = (NODE**) malloc(sizeof(NODE*) * (x->n - 1));
    for (i = 1; i < x->n; i++) {
      c->c[i - 1] = x->c[i];
      x->c[i]->r++;
      c->n++;
    }
    return c;
  }
  return new_node();
}

static void
add_sym(ENV *env, enum T t, const char* n, f_do f) {
  ITEM *ni;
  NODE *node;
  node = new_node();
  node->t = t;
  node->u.s = strdup(n);
  node->f = f;
  ni = (ITEM*) malloc(sizeof(ITEM));
  memset(ni, 0, sizeof(ITEM));
  ni->k = strdup(n);
  ni->v = node;
  env->lv = (ITEM**) realloc(env->lv, sizeof(ITEM*) * (env->nv + 1));
  env->lv[env->nv] = ni;
  env->nv++;
}

static void
add_defaults(ENV *env) {
  add_sym(env, NODE_CALL, "+", do_plus);
  add_sym(env, NODE_CALL, "-", do_minus);
  add_sym(env, NODE_CALL, "*", do_mul);
  add_sym(env, NODE_CALL, "/", do_div);
  add_sym(env, NODE_CALL, "1+", do_plus1);
  add_sym(env, NODE_CALL, "1-", do_minus1);
  add_sym(env, NODE_CALL, "not", do_not);
  add_sym(env, NODE_CALL, "mod", do_mod);
  add_sym(env, NODE_CALL, "if", do_if);
  add_sym(env, NODE_CALL, ">", do_gt);
  add_sym(env, NODE_CALL, ">=", do_ge);
  add_sym(env, NODE_CALL, "<", do_lt);
  add_sym(env, NODE_CALL, "<=", do_le);
  add_sym(env, NODE_CALL, "=", do_eq);
  add_sym(env, NODE_CALL, "eq", do_eq);
  add_sym(env, NODE_CALL, "print", do_print);
  add_sym(env, NODE_CALL, "println", do_println);
  add_sym(env, NODE_CALL, "princ", do_princ);
  add_sym(env, NODE_CALL, "quote", do_quote);
  add_sym(env, NODE_CALL, "setq", do_setq);
  add_sym(env, NODE_CALL, "defun", do_defun);
  add_sym(env, NODE_CALL, "progn", do_progn);
  add_sym(env, NODE_CALL, "cond", do_cond);
  add_sym(env, NODE_CALL, "car", do_car);
  add_sym(env, NODE_CALL, "cdr", do_cdr);
  add_sym(env, NODE_CALL, "dotimes", do_dotimes);
  add_sym(env, NODE_NIL, "nil", NULL);
  add_sym(env, NODE_T, "t", NULL);
}

static NODE*
eval_node(ENV *env, NODE *node) {
  static ENV *global;
  int i;
  if (global == NULL) {
    while (env->p) env = env->p;
    global = env;
  }

  switch (node->t) {
  case NODE_QUOTE:
    return eval_node(env, node->c[0]);
  case NODE_PROGN:
    return do_progn(env, node);
  case NODE_IDENT:
    return do_ident(env, node);
  case NODE_CALL:
    for (i = 0; i < global->nv; i++) {
      if (node->t == NODE_CALL && node->u.s && match(node->u.s, global->lv[i]->k, strlen(global->lv[i]->k))) {
        if (global->lv[i]->v->f) {
          return ((f_do)(global->lv[i]->v->f))(env, node);
        }
      }
    }
    return do_call(env, node);
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
  case NODE_LIST:
    node->r++;
    return node;
  }

  return new_error("unknown node");
}

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
    p = (char*) malloc(fsize + 1);
    pp = p;
    memset(p, 0, fsize+1);
    if (!fread(p, fsize, 1, fp)) {
      fprintf(stderr, "%s: %s\n", argv[0], strerror(errno));
      exit(1);
    }
    fclose(fp);

    env = new_env(NULL);
    add_defaults(env);
    top = new_node();
    top->t = NODE_PROGN;
    p = (char*) parse_args(env, top, p);
    if (!p) {
      free_node(top);
      free_env(env);
      exit(1);
    }
    p = (char*) skip_white((char*)p);
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
      if (!fgets(buf , sizeof(buf), stdin)) break;
    } else {
      fsize = fread(buf, 1, sizeof(buf), stdin);
      if (fsize <= 0) break;
      buf[fsize] = 0;
    }
    top = new_node();
    pp = parse_any(env, top, buf);
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
    } else if (isatty(fileno(stdin))) {
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
