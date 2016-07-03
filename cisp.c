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
  NODE_PLUS, NODE_MINUS, NODE_MUL, NODE_DIV,
  NODE_PLUS1, NODE_MINUS1,
  NODE_EQ,
  NODE_LT, NODE_LE,
  NODE_GT, NODE_GE,
  NODE_NOT, NODE_MOD,
  NODE_IF, NODE_DEFUN, NODE_CALL, NODE_DOTIMES,
  NODE_CAR, NODE_CDR,
  NODE_PRINT, NODE_PRINTLN, NODE_PRINC, NODE_SETQ,
  NODE_PROGN,
  NODE_COND,
  NODE_FORMAT,
  NODE_ERROR,
};

typedef struct _NODE {
  int t;
  union {
    long i;
    double d;
    char* s;
    void* p;
  } u;
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

static char*
raise(const char *p) {
  if (!p) return NULL;
  fprintf(stderr, "invalid token: %s\n", p);
  return NULL;
}

static const char* parse_any(NODE *node, const char *p, int q);
static const char* parse_paren(NODE *node, const char *p);
static const char* parse_ident(NODE *node, const char *p);
static NODE* eval_node(ENV *env, NODE *node);
static void print_node(size_t nbuf, char *buf, NODE *node, int mode);
static void free_node(NODE *node);

static const char*
skip_white(const char *p) {
  if (!p) return NULL;
  while (*p && isspace((int)*p)) p++;
  if (*p == ';') {
    p++;
    while (*p && *p != '\n') p++;
  }
  return p;
}

static NODE*
new_node() {
  NODE* node = (NODE*) malloc(sizeof(NODE));
  memset(node, 0, sizeof(NODE));
  node->t = NODE_NIL;
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
  while (p < e)
    if (*p++ != *rhs++) return 0;
  if (*rhs) return 0;
  return 1;
}

static const char*
parse_number(NODE *node, const char* p) {
  const char *t = p;
  if (*p == '-') p++;
  while (*p && isdigit(*p)) p++;
  if (*p != '.') {
    node->t = NODE_INT;
    node->u.i = atoi(t);
  } else if (*p) {
    p++;
    while (*p && isdigit(*p)) p++;
    node->t = NODE_DOUBLE;
    node->u.d = atof(t);
  }
  node->r++;
  return p;
}

static const char*
parse_args(NODE *node, const char *p) {
  if (!p) return NULL;
  p = skip_white(p);
  while (p && *p && *p != ')') {
    NODE *child = NULL;
    child = new_node();
    /* TODO: ugly */
    if ((node->t == NODE_DEFUN && node->n == 1) || (node->t == NODE_DOTIMES && node->n == 0)) {
      p = parse_any(child, p, 1);
    } else {
      p = parse_any(child, p, 0);
    }
    if (!p) {
      free_node(child);
      return NULL;
    }
    node->c = (NODE**) realloc(node->c, sizeof(NODE) * (node->n + 1));
    node->c[node->n] = child;
    node->n++;
    p = skip_white(p);
  }
  if (p && *p) {
    if (*p == ')') p++;
    else return raise(p);
  }
  return p;
}

static const char*
parse_paren(NODE *node, const char *p) {
  if (!p) return NULL;
  const char *t = p;
  while (!isspace(*p) && *p != ')') p++;
  if (match(t, "+", (size_t)(p - t))) node->t = NODE_PLUS;
  else if (match(t, "-", (size_t)(p - t))) node->t = NODE_MINUS;
  else if (match(t, "*", (size_t)(p - t))) node->t = NODE_MUL;
  else if (match(t, "/", (size_t)(p - t))) node->t = NODE_DIV;
  else if (match(t, "<", (size_t)(p - t))) node->t = NODE_LT;
  else if (match(t, "<=", (size_t)(p - t))) node->t = NODE_LE;
  else if (match(t, ">", (size_t)(p - t))) node->t = NODE_GT;
  else if (match(t, ">=", (size_t)(p - t))) node->t = NODE_GE;
  else if (match(t, "=", (size_t)(p - t))) node->t = NODE_EQ;
  else if (match(t, "1+", (size_t)(p - t))) node->t = NODE_PLUS1;
  else if (match(t, "1-", (size_t)(p - t))) node->t = NODE_MINUS1;
  else if (match(t, "not", (size_t)(p - t))) node->t = NODE_NOT;
  else if (match(t, "mod", (size_t)(p - t))) node->t = NODE_MOD;
  else if (match(t, "if", (size_t)(p - t))) node->t = NODE_IF;
  else if (match(t, "print", (size_t)(p - t))) node->t = NODE_PRINT;
  else if (match(t, "println", (size_t)(p - t))) node->t = NODE_PRINTLN;
  else if (match(t, "princ", (size_t)(p - t))) node->t = NODE_PRINC;
  else if (match(t, "quote", (size_t)(p - t))) node->t = NODE_QUOTE;
  else if (match(t, "setq", (size_t)(p - t))) node->t = NODE_SETQ;
  else if (match(t, "progn", (size_t)(p - t))) node->t = NODE_PROGN;
  else if (match(t, "cond", (size_t)(p - t))) node->t = NODE_COND;
  else if (match(t, "car", (size_t)(p - t))) node->t = NODE_CAR;
  else if (match(t, "cdr", (size_t)(p - t))) node->t = NODE_CDR;
  else if (match(t, "dotimes", (size_t)(p - t))) node->t = NODE_DOTIMES;
  else if (match(t, "defun", (size_t)(p - t))) node->t = NODE_DEFUN;
  else {
    p = parse_ident(node, t);
    node->t = NODE_CALL;
  }

  p = parse_args(node, p);
  if (p && *p && node->n == 0) return raise(p);

  switch (node->t) {
  case NODE_PLUS: if (node->n < 2) return raise(p); break;
  case NODE_MINUS: if (node->n < 2) return raise(p); break;
  case NODE_MUL: if (node->n < 2) return raise(p); break;
  case NODE_DIV: if (node->n < 2) return raise(p); break;
  case NODE_PLUS1: if (node->n != 1) return raise(p); break;
  case NODE_MINUS1: if (node->n != 1) return raise(p); break;
  case NODE_GT: if (node->n != 2) return raise(p); break;
  case NODE_GE: if (node->n != 2) return raise(p); break;
  case NODE_LT: if (node->n != 2) return raise(p); break;
  case NODE_LE: if (node->n != 2) return raise(p); break;
  case NODE_EQ: if (node->n != 2) return raise(p); break;
  case NODE_NOT: if (node->n != 1) return raise(p); break;
  case NODE_MOD: if (node->n != 2) return raise(p); break;
  case NODE_IF: if (node->n != 3) return raise(p); break;
  case NODE_PRINT: if (node->n != 1) return raise(p); break;
  case NODE_PRINTLN: if (node->n != 1) return raise(p); break;
  case NODE_PRINC: if (node->n != 1) return raise(p); break;
  case NODE_QUOTE: if (node->n != 1) return raise(p); break;
  case NODE_SETQ: if (node->n != 2) return raise(p); break;
  case NODE_PROGN: if (node->n < 1) return raise(p); break;
  case NODE_COND: if (node->n < 1) return raise(p); break;
  case NODE_CAR: if (node->n != 1) return raise(p); break;
  case NODE_CDR: if (node->n != 1) return raise(p); break;
  case NODE_DOTIMES: if (node->n != 2) return raise(p); break;
  case NODE_DEFUN: if (node->n < 3) return raise(p); break;
  }
  node->r++;
  return p;
}

static const char*
parse_ident(NODE *node, const char *p) {
  const char *t = p;
  while (*p && isalpha(*p)) p++;
  if (match(t, "nil", (size_t)(p - t))) {
    node->t = NODE_NIL;
    node->r++;
    return p;
  }
  if (match(t, "t", (size_t)(p - t))) {
    node->t = NODE_T;
    node->r++;
    return p;
  }
  while (*p && isdigit(*p)) p++;
  node->t = NODE_IDENT;
  node->u.s = (char*) malloc((size_t)(p - t) + 1);
  memset(node->u.s, 0, (size_t)(p - t) + 1);
  memcpy(node->u.s, t, (size_t)(p - t));
  node->r++;
  return p;
}

static const char*
parse_quote(NODE *node, const char *p) {
  NODE *child = NULL;
  child = new_node();
  if (*p == '(') {
    child->t = NODE_LIST;
    p = parse_args(child, p+1);
  } else {
    p = parse_ident(child, p);
  }
  if (!p) {
    free_node(child);
    return NULL;
  }
  node->t = NODE_QUOTE;
  node->c = (NODE**) realloc(node->c, sizeof(NODE) * (node->n + 1));
  node->c[node->n] = child;
  node->n++;
  node->r++;
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
  node->r++;
  return p;
}

static const char*
parse_any(NODE *node, const char *p, int q) {
  if (!p) return NULL;
  p = skip_white(p);
  if (*p == '(') {
    if (q) return parse_quote(node, p);
    return parse_paren(node, p + 1);
  }
  if (*p == '-' || isdigit(*p)) return parse_number(node, p);
  if (*p == '\'') return parse_quote(node, p + 1);
  if (*p == '"') return parse_string(node, p + 1);
  if (isalpha(*p)) return parse_ident(node, p);
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
  case NODE_PLUS: strncat(buf, "(+", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_MINUS: strncat(buf, "(-", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_MUL: strncat(buf, "(*", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_DIV: strncat(buf, "(/", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_PLUS1: strncat(buf, "(1+", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_MINUS1: strncat(buf, "(1-", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_EQ: strncat(buf, "(=", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_NOT: strncat(buf, "(not", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_MOD: strncat(buf, "(mod", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_IF: strncat(buf, "(if", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_GT: strncat(buf, "(>", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_GE: strncat(buf, "(>=", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_LT: strncat(buf, "(<", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_LE: strncat(buf, "(<=", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_PRINT: strncat(buf, "(print", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_PRINTLN: strncat(buf, "(println", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_PRINC: strncat(buf, "(princ", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_QUOTE: strncat(buf, "'", nbuf); print_node(nbuf, buf, node->c[0], mode); break;
  case NODE_LIST: strncat(buf, "(", nbuf); print_list(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_SETQ: strncat(buf, "(setq", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_DEFUN: strncat(buf, "(defun", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_PROGN: strncat(buf, "(progn", nbuf); print_args(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_COND: strncat(buf, "(cond", nbuf); print_list(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_CAR: strncat(buf, "(car", nbuf); print_list(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_CDR: strncat(buf, "(cdr", nbuf); print_list(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
  case NODE_DOTIMES: strncat(buf, "(dotimes", nbuf); print_list(nbuf, buf, node, mode); strncat(buf, ")", nbuf); break;
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
    switch (node->t) {
    case NODE_STRING:
    case NODE_IDENT:
    case NODE_CALL:
    case NODE_ERROR:
      free(node->u.s);
      break;
    }
    free(node);
  }
}

static void
free_env(ENV *env) {
  free(env->lv);
  free(env->lf);
  free(env);
}

static long
int_value(ENV *env, NODE *node) {
  node = eval_node(env, node);
  switch (node->t) {
  case NODE_NIL: return 0; break;
  case NODE_T: return 1; break;
  case NODE_INT: return node->u.i; break;
  case NODE_DOUBLE: return (long)node->u.d; break;
  case NODE_QUOTE: return int_value(env, node->c[0]); break;
  }
  return 0;
}

static double
double_value(ENV *env, NODE *node) {
  node = eval_node(env, node);
  switch (node->t) {
  case NODE_INT: return (double)node->u.i; break;
  case NODE_DOUBLE: return node->u.d; break;
  }
  return 0.0;
}

static NODE*
look_ident(ENV *env, const char *k) {
  int i;

  for (i = 0; i < env->nv; i++) {
    if (!strcmp(env->lv[i]->k, k)) {
      return env->lv[i]->v;
    }
  }

  if (env->p) return look_ident(env->p, k);
/* TODO: bottle neck */
/*
  static ENV *global;
  if (global == NULL) {
    while (env->p) env = env->p;
    global = env;
  } else {
    env = global;
  }

  for (i = 0; i < env->nv; i++) {
    if (!strcmp(env->lv[i]->k, k)) {
      return env->lv[i]->v;
    }
  }
*/
  return new_errorf("unknown variable: %s", k);
}

static NODE*
look_func(ENV *env, const char *k) {
  static ENV *global;
  int i;

  if (global == NULL) {
    while (env->p) env = env->p;
    global = env;
  } else {
    env = global;
  }
  for (i = 0; i < env->nf; i++) {
    if (!strcmp(env->lf[i]->k, k)) {
      return env->lf[i]->v;
    }
  }
  return NULL;
}

static NODE*
eval_node(ENV *env, NODE *node) {
  ENV *newenv;
  NODE *nn, *c, *x;
  ITEM *ni;
  int i, r;
  char buf[BUFSIZ];

  switch (node->t) {
  case NODE_PLUS:
    nn = new_node();
    for (i = 0; i < node->n; i++) {
      c = eval_node(env, node->c[i]);
      if (i == 0) {
        nn->t = c->t;
        nn->u = c->u;
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
    }
    nn->r++;
    return nn;
  case NODE_MINUS:
    nn = new_node();
    for (i = 0; i < node->n; i++) {
      c = eval_node(env, node->c[i]);
      if (i == 0) {
        nn->t = c->t;
        nn->u = c->u;
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
    }
    nn->r++;
    return nn;
  case NODE_MUL:
    nn = new_node();
    for (i = 0; i < node->n; i++) {
      c = eval_node(env, node->c[i]);
      if (i == 0) {
        nn->u = c->u;
        nn->t = c->t;
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
    }
    nn->r++;
    return nn;
  case NODE_DIV:
    nn = new_node();
    for (i = 0; i < node->n; i++) {
      c = eval_node(env, node->c[i]);
      if (i == 0) {
        nn->t = c->t;
        nn->u = c->u;
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
    }
    nn->r++;
    return nn;
  case NODE_PLUS1:
    c = new_node();
    x = eval_node(env, node->c[0]);
    c->t = x->t;
    c->t = x->t;
    switch (c->t) {
    case NODE_INT: c->u.i = x->u.i + 1; break;
    case NODE_DOUBLE: c->u.d = x->u.i + 1.0; break;
    default: break;
    }
    return c;
  case NODE_MINUS1:
    c = new_node();
    x = eval_node(env, node->c[0]);
    c->t = x->t;
    c->t = x->t;
    switch (c->t) {
    case NODE_INT: c->u.i = x->u.i - 1; break;
    case NODE_DOUBLE: c->u.d = x->u.i - 1.0; break;
    default: break;
    }
    return c;
  case NODE_NOT:
    c = new_node();
    c->t = NODE_INT;
    c->u.i = !int_value(env, node->c[0]);
    return c;
  case NODE_MOD:
    c = new_node();
    c->t = NODE_INT;
    c->u.i = int_value(env, node->c[0]) % int_value(env, node->c[1]);
    return c;
  case NODE_IF:
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
    c =  eval_node(env, node->c[r > 0 ? 1 : 2]);
    c->r++;
    return c;
  case NODE_GT:
    nn = new_node();
    nn->t = NODE_INT;
    nn->u.i = double_value(env, node->c[0]) > double_value(env, node->c[1]);
    nn->r++;
    return nn;
  case NODE_GE:
    nn = new_node();
    nn->t = NODE_INT;
    nn->u.i = double_value(env, node->c[0]) >= double_value(env, node->c[1]);
    nn->r++;
    return nn;
  case NODE_LT:
    nn = new_node();
    nn->t = NODE_INT;
    nn->u.i = double_value(env, node->c[0]) < double_value(env, node->c[1]);
    nn->r++;
    return nn;
  case NODE_LE:
    nn = new_node();
    nn->t = NODE_INT;
    nn->u.i = double_value(env, node->c[0]) <= double_value(env, node->c[1]);
    nn->r++;
    return nn;
  case NODE_EQ:
    nn = new_node();
    nn->t = NODE_INT;
    /* TODO: string comparison */
    nn->u.i = int_value(env, node->c[0]) == int_value(env, node->c[1]);
    nn->r++;
    return nn;
  case NODE_PRINT:
    c = eval_node(env, node->c[0]);
    buf[0] = 0;
    print_node(sizeof(buf), buf, c, 0);
    puts(buf);
    c->r++;
    return c;
  case NODE_PRINTLN:
    c = eval_node(env, node->c[0]);
    buf[0] = 0;
    print_node(sizeof(buf), buf, c, 0);
    puts(buf);
    c->r++;
    return c;
  case NODE_PRINC:
    c = eval_node(env, node->c[0]);
    buf[0] = 0;
    print_node(sizeof(buf), buf, c, 1);
    puts(buf);
    c->r++;
    return c;
  case NODE_QUOTE:
    node->r++;
    return node;
  case NODE_SETQ:
    x = node->c[0];
    if (x->t != NODE_IDENT) {
      buf[0] = 0;
      print_node(sizeof(buf), buf, x, 0);
      return new_errorf("invalid identifier: %s", buf);
    }
    ni = (ITEM*) malloc(sizeof(ITEM));
    memset(ni, 0, sizeof(ITEM));
    ni->k = x->u.s;
    ni->v = node->c[1];
    ni->v->r++;
    env->lv = (ITEM**) realloc(env->lv, sizeof(ITEM) * (env->nv + 1));
    env->lv[env->nv] = ni;
    env->nv++;
    node->c[1]->r++;
    return node->c[1];
  case NODE_IDENT:
    return look_ident(env, node->u.s);
  case NODE_CALL:
    x = look_func(env, node->u.s);
    if (!x) {
      return new_errorf("unknown function: %s", node->u.s);
    }
    newenv = new_env(env);
    c = x->c[1]->c[0];
    for (i = 0; i < node->n; i++) {
      ni = (ITEM*) malloc(sizeof(ITEM));
      memset(ni, 0, sizeof(ITEM));
      ni->k = c->c[i]->u.s;
      ni->v = eval_node(env, node->c[i]);
      ni->v->r++;
      newenv->lv = (ITEM**) realloc(newenv->lv, sizeof(ITEM) * (newenv->nv + 1));
      newenv->lv[newenv->nv] = ni;
      newenv->nv++;
    }
    c = NULL;
    for (i = 2; i < x->n; i++) {
      c = eval_node(newenv, x->c[i]);
    }
    free_env(newenv);
    return c;
  case NODE_DEFUN:
    x = node->c[0];
    if (x->t != NODE_IDENT) {
      buf[0] = 0;
      print_node(sizeof(buf), buf, x, 0);
      return new_errorf("invalid identifier: %s", buf);
    }
    ni = (ITEM*) malloc(sizeof(ITEM));
    memset(ni, 0, sizeof(ITEM));
    ni->k = x->u.s;
    ni->v = node;
    ni->v->r++;
    env->lf = (ITEM**) realloc(env->lf, sizeof(ITEM) * (env->nf + 1));
    env->lf[env->nf] = ni;
    env->nf++;
    node->c[1]->r++;
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
  case NODE_LIST:
    node->r++;
    return node;
  case NODE_PROGN:
    c = NULL;
    for (i = 0; i < node->n; i++) {
      c = eval_node(env, node->c[i]);
    }
    if (c) {
      c->r++;
      return c;
    }
    return new_node();
  case NODE_COND:
    c = NULL;
    for (i = 0; i < node->n; i++) {
      if (node->c[i]->t != NODE_LIST && node->c[i]->n != 2)
        return new_error("cond should have condition list");
      if (int_value(env, eval_node(env, node->c[i]->c[0])) != 0) {
        return eval_node(env, node->c[i]->c[1]);
      }
    }
    if (c) {
      c->r++;
      return c;
    }
    return new_node();
  case NODE_CAR:
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
  case NODE_CDR:
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
      c = x->c[x->n-1];
      c->r++;
      return c;
    }
    return new_node();
  case NODE_DOTIMES:
    x = node->c[0]->c[0];
    if (x->t != NODE_LIST || x->n != 2) {
      buf[0] = 0;
      print_node(sizeof(buf), buf, x, 0);
      return new_errorf("invalid defintion: %s", buf);
    }
    if (x->c[0]->t != NODE_IDENT) {
      buf[0] = 0;
      print_node(sizeof(buf), buf, x->c[0], 0);
      return new_errorf("invalid identifier: %s", buf);
    }
    r = int_value(env, eval_node(env, x->c[1]));
    newenv = new_env(env);
    ni = (ITEM*) malloc(sizeof(ITEM));
    memset(ni, 0, sizeof(ITEM));
    ni->k = x->c[0]->u.s;
    ni->v = new_node();
    ni->v->t = NODE_INT;
    ni->v->r++;
    newenv->lv = (ITEM**) realloc(newenv->lv, sizeof(ITEM) * (newenv->nv + 1));
    newenv->lv[newenv->nv] = ni;
    newenv->nv++;
    c = NULL;
    for (i = 0; i < r; i++) {
      ni->v->u.i = i;
      c = eval_node(newenv, node->c[1]);
    }
    free_env(newenv);
    return c;
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
    memset(p, 0, fsize+1);
    if (!fread(p, fsize, 1, fp)) {
      fprintf(stderr, "%s: %s\n", argv[0], strerror(errno));
      exit(1);
    }
    fclose(fp);

    top = new_node();
    top->t = NODE_PROGN;
    if (!parse_args(top, p)) {
      exit(1);
    }
    free(p);
    env = new_env(NULL);
    ret = eval_node(env, top);
    if (ret->t == NODE_ERROR)
      fprintf(stderr, "%s: %s\n", argv[0], ret->u.s);
    free_node(ret);
    free_node(top);
    free_env(env);
    exit(0);
  }

  env = new_env(NULL);
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
    pp = parse_any(top, buf, 0);
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
