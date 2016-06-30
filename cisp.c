#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <ctype.h>

enum T {
  NODE_INT, NODE_DOUBLE, NODE_STRING,
  NODE_PLUS, NODE_MINUS, NODE_MUL, NODE_DIV,
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

static void
raise(const char *p) {
  fprintf(stderr, "invalid token: %s\n", p);
  exit(1);
}

static const char* parse_any(NODE *node, const char *p);
static const char* parse_paren(NODE *node, const char *p);
static void print_node(NODE *node);

static const char*
skip_white(const char *p) {
  while (isspace((int)*p)) p++;
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
  while (*p && isdigit(*p)) {
    p++;
  }
  if (*p != '.') {
    node->t = NODE_INT;
    node->u.i = atoi(t);
  } else {
    p++;
    while (*p && isdigit(*p)) {
      p++;
    }
    node->t = NODE_DOUBLE;
    node->u.d = atof(t);
  }
  return p;
}

static const char*
parse_args(NODE *node, const char *p) {
  p = skip_white(p);
  while (*p && *p != ')') {
    NODE *child = NULL;
    child = new_node();
    p = parse_any(child, p);
    node->c = (NODE**) realloc(node->c, sizeof(NODE) * (node->n + 1));
    node->c[node->n] = child;
    node->n++;
    p = skip_white(p);
  }
  if (*p == ')') p++;
  else raise(p);
  return p;
}

static const char*
parse_paren(NODE *node, const char *p) {
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
  } else
    raise(t);
  if (*p != ')') {
    p = parse_args(node, p);
  }
  return p;
}

static const char*
parse_any(NODE *node, const char *p) {
  p = skip_white(p);
  if (*p == '(') return parse_paren(node, p + 1); 
  if (isdigit(*p)) return parse_number(node, p);
  if (*p) raise(p);
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
  case NODE_PLUS:
    printf("(+");
    print_args(node);
    printf(")");
    break;
  case NODE_MINUS:
    printf("(-");
    print_args(node);
    printf(")");
    break;
  case NODE_MUL:
    printf("(*");
    print_args(node);
    printf(")");
    break;
  case NODE_DIV:
    printf("(/");
    print_args(node);
    printf(")");
    break;
  case NODE_INT:
    printf("%ld", node->u.i);
    break;
  case NODE_DOUBLE:
    printf("%f", node->u.d);
	break;
  }
}

static void
free_node(NODE *node) {
  int i;
  switch (node->t) {
  case NODE_PLUS:
    for (i = 0; i < node->n; i++)
      free_node(node->c[i]);
    free(node);
    break;
  case NODE_INT:
    node->r--;
    if (node->r <= 0) free(node);
    break;
  case NODE_DOUBLE:
    node->r--;
    if (node->r <= 0) free(node);
    break;
  }
}

static NODE*
eval_node(NODE *node) {
  NODE *nn, *c;
  int i;
  switch (node->t) {
  case NODE_PLUS:
    nn = new_node();
    for (i = 0; i < node->n; i++) {
      c = eval_node(node->c[i]);
      if (i == 0) {
        nn->u.d = c->u.d;
        continue;
      }
      switch (c->t) {
      case NODE_INT:
        if (nn->t == NODE_INT) nn->u.i += (long)c->u.i;
        else nn->u.d += (double)c->u.i;
        break;
      case NODE_DOUBLE:
        if (nn->t == NODE_INT) nn->u.i += (long)c->u.d;
        else nn->u.d += (double)c->u.d;
        break;
      default:
        break;
      }
    }
    nn->r++;
    return nn;
  case NODE_MINUS:
    nn = new_node();
    for (i = 0; i < node->n; i++) {
      c = eval_node(node->c[i]);
      if (i == 0) {
        nn->u.d = c->u.d;
        continue;
      }
      switch (c->t) {
      case NODE_INT:
        if (nn->t == NODE_INT) nn->u.i -= (long)c->u.i;
        else nn->u.d += (double)c->u.i;
        break;
      case NODE_DOUBLE:
        if (nn->t == NODE_INT) nn->u.i -= (long)c->u.d;
        else nn->u.d += (double)c->u.d;
        break;
      default:
        break;
      }
    }
    nn->r++;
    return nn;
  case NODE_MUL:
    nn = new_node();
    for (i = 0; i < node->n; i++) {
      c = eval_node(node->c[i]);
      if (i == 0) {
        nn->u.d = c->u.d;
        continue;
      }
      switch (c->t) {
      case NODE_INT:
        if (nn->t == NODE_INT) nn->u.i *= (long)c->u.i;
        else nn->u.d += (double)c->u.i;
        break;
      case NODE_DOUBLE:
        if (nn->t == NODE_INT) nn->u.i *= (long)c->u.d;
        else nn->u.d += (double)c->u.d;
        break;
      default:
        break;
      }
    }
    nn->r++;
    return nn;
  case NODE_DIV:
    nn = new_node();
    for (i = 0; i < node->n; i++) {
      c = eval_node(node->c[i]);
      if (i == 0) {
        nn->u.d = c->u.d;
        continue;
      }
      switch (c->t) {
      case NODE_INT:
        if (nn->t == NODE_INT) nn->u.i /= (long)c->u.i;
        else nn->u.d += (double)c->u.i;
        break;
      case NODE_DOUBLE:
        if (nn->t == NODE_INT) nn->u.i /= (long)c->u.d;
        else nn->u.d += (double)c->u.d;
        break;
      default:
        break;
      }
    }
    nn->r++;
    return nn;
  case NODE_INT:
    node->r++;
    return node;
  case NODE_DOUBLE:
    node->r++;
    return node;
  }
  return new_node();
}

int
main(int argc, char* argv[]) {
  NODE *top, *ret;
  char buf[BUFSIZ];

  if (argc > 1) {
    top = new_node(), *ret;
    parse_any(top, argv[1]);
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
    parse_any(top, buf);
    ret = eval_node(top);
    print_node(ret);
    puts("");
    free_node(ret);
    free_node(top);
  }
  return 0;
}
