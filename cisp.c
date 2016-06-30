#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <ctype.h>

enum T {
  NODE_NIL, NODE_INT, NODE_DOUBLE, NODE_STRING,
  NODE_PLUS, NODE_MINUS, NODE_MUL, NODE_DIV,
  NODE_IF,
  NODE_PRINT,
  NODE_LT, NODE_LE,
  NODE_GT, NODE_GE,
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

static char*
raise(const char *p) {
  if (!p) return NULL;
  fprintf(stderr, "invalid token: %s\n", p);
  return NULL;
}

static const char* parse_any(NODE *node, const char *p);
static const char* parse_paren(NODE *node, const char *p);
static void print_node(NODE *node);
static void free_node(NODE *node);

static const char*
skip_white(const char *p) {
  if (p) while (isspace((int)*p)) p++;
  return p;
}

static NODE*
new_node() {
  NODE* node = (NODE*) malloc(sizeof(NODE));
  memset(node, 0, sizeof(NODE));
  node->t = NODE_INT;
  return node;
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
    p = parse_any(child, p);
    if (!p) {
      free_node(child);
      return NULL;
    }
    node->c = (NODE**) realloc(node->c, sizeof(NODE) * (node->n + 1));
    node->c[node->n] = child;
    node->n++;
    p = skip_white(p);
  }
  if (p && *p == ')') p++;
  else return raise(p);
  return p;
}

static const char*
parse_paren(NODE *node, const char *p) {
  if (!p) return NULL;
  const char *t = p;
  while (!isspace(*p) && *p != ')') {
    p++;
  }
  if (!strncmp(t, "+", (size_t)(p - t))) {
    node->t = NODE_PLUS;
  } else if (!strncmp(t, "-", (size_t)(p - t))) {
    node->t = NODE_MINUS;
  } else if (!strncmp(t, "*", (size_t)(p - t))) {
    node->t = NODE_MUL;
  } else if (!strncmp(t, "/", (size_t)(p - t))) {
    node->t = NODE_DIV;
  } else if (!strncmp(t, "if", (size_t)(p - t))) {
    node->t = NODE_IF;
  } else if (!strncmp(t, "print", (size_t)(p - t))) {
    node->t = NODE_PRINT;
  } else if (!strncmp(t, "<", (size_t)(p - t))) {
    node->t = NODE_GT;
  } else if (!strncmp(t, "<=", (size_t)(p - t))) {
    node->t = NODE_GE;
  } else if (!strncmp(t, ">", (size_t)(p - t))) {
    node->t = NODE_LT;
  } else if (!strncmp(t, ">=", (size_t)(p - t))) {
    node->t = NODE_LE;
  } else return raise(t);
  p = parse_args(node, p);
  if (p && *p && node->n == 0) raise(p);
  return p;
}

static const char*
parse_any(NODE *node, const char *p) {
  if (!p) return NULL;
  p = skip_white(p);
  if (*p == '(') return parse_paren(node, p + 1); 
  if (*p == '-' || isdigit(*p)) return parse_number(node, p);
  if (*p) return raise(p);
  return p;
}

static void
print_args(NODE *node) {
  int i;
  for (i = 0; i < node->n; i++) {
    printf(" ");
    print_node(node->c[i]);
  }
}

static void
print_node(NODE *node) {
  switch (node->t) {
  case NODE_PLUS: printf("(+"); print_args(node); printf(")"); break;
  case NODE_MINUS: printf("(-"); print_args(node); printf(")"); break;
  case NODE_MUL: printf("(*"); print_args(node); printf(")"); break;
  case NODE_DIV: printf("(/"); print_args(node); printf(")"); break;
  case NODE_IF: printf("(if"); print_args(node); printf(")"); break;
  case NODE_GT: printf("(>"); print_args(node); printf(")"); break;
  case NODE_GE: printf("(>="); print_args(node); printf(")"); break;
  case NODE_LT: printf("(<"); print_args(node); printf(")"); break;
  case NODE_LE: printf("(<="); print_args(node); printf(")"); break;
  case NODE_INT: printf("%ld", node->u.i); break;
  case NODE_DOUBLE: printf("%f", node->u.d); break;
  case NODE_NIL: printf("nil"); break;
  case NODE_PRINT: printf("print"); break;
  }
}

static void
free_node(NODE *node) {
  int i;
  node->r--;
  if (node->r <= 0) {
    for (i = 0; i < node->n; i++)
      free_node(node->c[i]);
    free(node);
  }
}

static long
int_value(NODE *node) {
  switch (node->t) {
  case NODE_INT: return node->u.i; break;
  case NODE_DOUBLE: return (long)node->u.d; break;
  }
  return 0;
}

static double
double_value(NODE *node) {
  switch (node->t) {
  case NODE_INT: return (double)node->u.i; break;
  case NODE_DOUBLE: return node->u.d; break;
  }
  return 0.0;
}

static NODE*
eval_node(NODE *node) {
  NODE *nn, *c;
  int i, r;
  switch (node->t) {
  case NODE_PLUS:
    nn = new_node();
    for (i = 0; i < node->n; i++) {
      c = eval_node(node->c[i]);
      if (i == 0) {
        nn->t = c->t;
        nn->u = c->u;
        continue;
      }
      switch (nn->t) {
      case NODE_INT:
        if (c->t == NODE_DOUBLE) {
          nn->u.d = double_value(nn) + double_value(c);
          nn->t = c->t;
        } else
          nn->u.i += int_value(c);
        break;
      case NODE_DOUBLE: nn->u.d += double_value(c); break;
      default: break;
      }
    }
    nn->r++;
    return nn;
  case NODE_MINUS:
    nn = new_node();
    for (i = 0; i < node->n; i++) {
      c = eval_node(node->c[i]);
      if (i == 0) {
        nn->t = c->t;
        nn->u = c->u;
        continue;
      }
      switch (nn->t) {
      case NODE_INT:
        if (c->t == NODE_DOUBLE) {
          nn->u.d = double_value(nn) - double_value(c);
          nn->t = c->t;
        } else
          nn->u.i -= int_value(c);
        break;
      case NODE_DOUBLE: nn->u.d -= double_value(c); break;
      default: break;
      }
    }
    nn->r++;
    return nn;
  case NODE_MUL:
    nn = new_node();
    for (i = 0; i < node->n; i++) {
      c = eval_node(node->c[i]);
      if (i == 0) {
        nn->u = c->u;
        nn->t = c->t;
        continue;
      }
      switch (nn->t) {
      case NODE_INT:
        if (c->t == NODE_DOUBLE) {
          nn->u.d = double_value(nn) * double_value(c);
          nn->t = c->t;
        } else
          nn->u.i *= int_value(c);
        break;
      case NODE_DOUBLE: nn->u.d *= double_value(c); break;
      default: break;
      }
    }
    nn->r++;
    return nn;
  case NODE_DIV:
    nn = new_node();
    for (i = 0; i < node->n; i++) {
      c = eval_node(node->c[i]);
      if (i == 0) {
        nn->t = c->t;
        nn->u = c->u;
        continue;
      }
      switch (nn->t) {
      case NODE_INT:
        if (c->t == NODE_DOUBLE) {
          nn->u.d = double_value(nn) / double_value(c);
          nn->t = c->t;
        } else
          nn->u.i /= int_value(c);
        break;
      case NODE_DOUBLE: nn->u.d /= double_value(c); break;
      default: break;
      }
    }
    nn->r++;
    return nn;
  case NODE_IF:
    c = eval_node(node->c[0]);
    switch (c->t) {
    case NODE_NIL:
      r = 0;
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
    if (r) {
      if (node->n == 1) return new_node();
      if (node->n == 2) {
        c =  node->c[1];
        c->r++;
        return c;
      }
      return new_node();
    }
    c = node->c[2];
    c->r++;
    return c;
  /* TODO: GT GE LT LE */
  case NODE_PRINT:
    print_node(node);
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
  }
  return new_node();
}

int
main(int argc, char* argv[]) {
  NODE *top, *ret;
  char buf[BUFSIZ], *p;

  if (argc > 1) {
    top = new_node(), *ret;
    if (!parse_any(top, argv[1])) {
      exit(1);
    }
    ret = eval_node(top);
    print_node(ret);
    puts("");
    free_node(ret);
    free_node(top);
    exit(0);
  }

  while (1) {
    printf("> ");
    if (!fgets(buf , sizeof(buf), stdin)) break;
    top = new_node();
    if (!parse_any(top, buf)) {
      continue;
    }
    ret = eval_node(top);
    print_node(ret);
    puts("");
    free_node(ret);
    free_node(top);
  }
  return 0;
}
