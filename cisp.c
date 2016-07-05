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
  return p;
}

static const char*
parse_args(NODE *node, const char *p) {
  if (!p) return NULL;
  p = skip_white(p);
  while (p && *p && *p != ')') {
    NODE *child = new_node();
    /* TODO: ugly */
    if ((node->t == NODE_DEFUN && node->n == 1) ||
        (node->t == NODE_DOTIMES && node->n == 0) ||
        (node->t == NODE_COND)
    ) {
      p = parse_any(child, p, 1);
    } else {
      p = parse_any(child, p, 0);
    }
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
  return p;
}

static const char*
parse_ident(NODE *node, const char *p) {
  const char *t = p;
  while (*p && isalpha(*p)) p++;
  if (match(t, "nil", (size_t)(p - t))) {
    node->t = NODE_NIL;
    return p;
  }
  if (match(t, "t", (size_t)(p - t))) {
    node->t = NODE_T;
    return p;
  }
  while (*p && isdigit(*p)) p++;
  node->t = NODE_IDENT;
  node->u.s = (char*) malloc((size_t)(p - t) + 1);
  memset(node->u.s, 0, (size_t)(p - t) + 1);
  memcpy(node->u.s, t, (size_t)(p - t));
  return p;
}

static const char*
parse_quote(NODE *node, const char *p) {
  NODE *child = new_node();
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
  node->c = (NODE**) realloc(node->c, sizeof(NODE*) * (node->n + 1));
  node->c[node->n] = child;
  node->n++;
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
    free(node->c);
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
  int i;
  for (i = 0; i < env->nv; i++) {
    free_node(env->lv[i]->v);
    free(env->lv[i]);
  }
  free(env->lv);
  for (i = 0; i < env->nf; i++) {
    free_node(env->lf[i]->v);
    free(env->lf[i]);
  }
  free(env->lf);
  free(env);
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
  nn->t = NODE_INT;
  nn->u.i = double_value(env, node->c[0]) > double_value(env, node->c[1]);
  return nn;
}

static NODE*
do_ge(ENV *env, NODE *node) {
  NODE *nn;

  if (node->n != 2) return new_errorf("malformed >=");
  nn = new_node();
  nn->t = NODE_INT;
  nn->u.i = double_value(env, node->c[0]) >= double_value(env, node->c[1]);
  return nn;
}

static NODE*
do_lt(ENV *env, NODE *node) {
  NODE *nn;

  if (node->n != 2) return new_errorf("malformed <");
  nn = new_node();
  nn->t = NODE_INT;
  nn->u.i = double_value(env, node->c[0]) < double_value(env, node->c[1]);
  return nn;
}

static NODE*
do_le(ENV *env, NODE *node) {
  NODE *nn;

  if (node->n != 2) return new_errorf("malformed <=");
  nn = new_node();
  nn->t = NODE_INT;
  nn->u.i = double_value(env, node->c[0]) <= double_value(env, node->c[1]);
  return nn;
}

static NODE*
do_eq(ENV *env, NODE *node) {
  NODE *nn;

  if (node->n != 2) return new_errorf("malformed =");
  nn = new_node();
  nn->t = NODE_INT;
  /* TODO: string comparison */
  nn->u.i = int_value(env, node->c[0]) == int_value(env, node->c[1]);
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
  ITEM *ni;
  char buf[BUFSIZ];

  if (node->n != 2) return new_errorf("malformed setq");
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
  env->lv = (ITEM**) realloc(env->lv, sizeof(ITEM*) * (env->nv + 1));
  env->lv[env->nv] = ni;
  env->nv++;
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
      x = env->lv[i]->v;
      x->r++;
      return x;
    }
  }
*/
  return new_errorf("unknown variable: %s", node->u.s);
}

static NODE*
look_func(ENV *env, const char *k) {
  NODE *x;
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
  NODE *x, *c;
  ITEM *ni;
  int i, j;

  x = look_func(env, node->u.s);
  if (!x) {
	return new_errorf("unknown function: %s", node->u.s);
  }
  newenv = new_env(env);
  c = x->c[1]->c[0];
  for (i = 0; i < node->n && i < c->n; i++) {
	for (j = 0; j < newenv->nv; j++) {
	  if (!strcmp(c->c[i]->u.s, newenv->lv[j]->k)) {
		free_env(newenv);
		free_node(x);
		return new_errorf("duplicated argument identifier %s", node->u.s);
	  }
	}
	ni = (ITEM*) malloc(sizeof(ITEM));
	memset(ni, 0, sizeof(ITEM));
	ni->k = c->c[i]->u.s;
	ni->v = eval_node(env, node->c[i]);
	newenv->lv = (ITEM**) realloc(newenv->lv, sizeof(ITEM*) * (newenv->nv + 1));
	newenv->lv[newenv->nv] = ni;
	newenv->nv++;
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
  ITEM *ni;
  char buf[BUFSIZ];

  if (node->n < 3) return new_errorf("malformed defun");
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
  env->lf = (ITEM**) realloc(env->lf, sizeof(ITEM*) * (env->nf + 1));
  env->lf[env->nf] = ni;
  env->nf++;
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
  NODE *x, *c;
  ITEM *ni;
  int i, r;
  char buf[BUFSIZ];

  if (node->n != 2) return new_errorf("malformed dotimes");
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
  c = eval_node(env, x->c[1]);
  r = int_value(env, c);
  free_node(c);
  newenv = new_env(env);
  ni = (ITEM*) malloc(sizeof(ITEM));
  memset(ni, 0, sizeof(ITEM));
  ni->k = x->c[0]->u.s;
  ni->v = new_node();
  ni->v->t = NODE_INT;
  newenv->lv = (ITEM**) realloc(newenv->lv, sizeof(ITEM*) * (newenv->nv + 1));
  newenv->lv[newenv->nv] = ni;
  newenv->nv++;
  c = NULL;
  for (i = 0; i < r; i++) {
	ni->v->u.i = i;
	if (c) free_node(c);
	c = eval_node(newenv, node->c[1]);
  }
  free_env(newenv);
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
	if (x->t != NODE_LIST)
	  return new_error("cond should have condition list");
	if (x->n != 2)
	  return new_error("cond should have pair of condition/value");
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

static NODE*
eval_node(ENV *env, NODE *node) {
  switch (node->t) {
  case NODE_PLUS:
    return do_plus(env, node);
  case NODE_MINUS:
    return do_minus(env, node);
  case NODE_MUL:
    return do_mul(env, node);
  case NODE_DIV:
    return do_div(env, node);
  case NODE_PLUS1:
    return do_plus1(env, node);
  case NODE_MINUS1:
    return do_minus1(env, node);
  case NODE_NOT:
    return do_not(env, node);
  case NODE_MOD:
    return do_mod(env, node);
  case NODE_IF:
    return do_if(env, node);
  case NODE_GT:
    return do_gt(env, node);
  case NODE_GE:
    return do_ge(env, node);
  case NODE_LT:
    return do_lt(env, node);
  case NODE_LE:
    return do_le(env, node);
  case NODE_EQ:
    return do_eq(env, node);
  case NODE_PRINT:
    return do_print(env, node);
  case NODE_PRINTLN:
    return do_println(env, node);
  case NODE_PRINC:
    return do_princ(env, node);
  case NODE_QUOTE:
    return do_quote(env, node);
  case NODE_SETQ:
    return do_setq(env, node);
  case NODE_IDENT:
    return do_ident(env, node);
  case NODE_CALL:
    return do_call(env, node);
  case NODE_DEFUN:
    return do_defun(env, node);
  case NODE_PROGN:
    return do_progn(env, node);
  case NODE_COND:
    return do_cond(env, node);
  case NODE_CAR:
    return do_car(env, node);
  case NODE_CDR:
    return do_cdr(env, node);
  case NODE_DOTIMES:
    return do_dotimes(env, node);
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

    top = new_node();
    top->t = NODE_PROGN;
    p = (char*) parse_args(top, p);
    if (!p) {
      exit(1);
    }
    p = (char*) skip_white((char*)p);
    if (*p) {
      raise(p);
      exit(1);
    }
    free((char*)pp);
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
