#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <ctype.h>

enum T {
  NODE_NIL, NODE_INT, NODE_DOUBLE, NODE_STRING, NODE_QUOTE, NODE_IDENT, NODE_LIST, NODE_PROGN, NODE_CALL,
  NODE_PLUS, NODE_MINUS, NODE_MUL, NODE_DIV,
  NODE_PLUS1, NODE_MINUS1,
  NODE_IF, NODE_DEFUN,
  NODE_PRINT, NODE_SETQ,
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

static ENV*
new_env(ENV *p) {
  ENV* env = (ENV*) malloc(sizeof(ENV));
  memset(env, 0, sizeof(ENV));
  env->p = p;
  return env;
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
    if (node->t == NODE_DEFUN && node->n == 1) {
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
  if (!strncmp(t, "+", (size_t)(p - t))) node->t = NODE_PLUS;
  else if (!strncmp(t, "-", (size_t)(p - t))) node->t = NODE_MINUS;
  else if (!strncmp(t, "*", (size_t)(p - t))) node->t = NODE_MUL;
  else if (!strncmp(t, "/", (size_t)(p - t))) node->t = NODE_DIV;
  else if (!strncmp(t, "<", (size_t)(p - t))) node->t = NODE_LT;
  else if (!strncmp(t, "<=", (size_t)(p - t))) node->t = NODE_LE;
  else if (!strncmp(t, ">", (size_t)(p - t))) node->t = NODE_GT;
  else if (!strncmp(t, ">=", (size_t)(p - t))) node->t = NODE_GE;
  else if (!strncmp(t, "1+", (size_t)(p - t))) node->t = NODE_PLUS1;
  else if (!strncmp(t, "1-", (size_t)(p - t))) node->t = NODE_MINUS1;
  else if (!strncmp(t, "if", (size_t)(p - t))) node->t = NODE_IF;
  else if (!strncmp(t, "print", (size_t)(p - t))) node->t = NODE_PRINT;
  else if (!strncmp(t, "quote", (size_t)(p - t))) node->t = NODE_QUOTE;
  else if (!strncmp(t, "setq", (size_t)(p - t))) node->t = NODE_SETQ;
  else if (!strncmp(t, "defun", (size_t)(p - t))) node->t = NODE_DEFUN;
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
  case NODE_IF: if (node->n != 3) return raise(p); break;
  case NODE_PRINT: if (node->n != 1) return raise(p); break;
  case NODE_QUOTE: if (node->n != 1) return raise(p); break;
  case NODE_SETQ: if (node->n != 2) return raise(p); break;
  case NODE_DEFUN: if (node->n < 3) return raise(p); break;
  }
  node->r++;
  return p;
}

static const char*
parse_ident(NODE *node, const char *p) {
  const char *t = p;
  while (*p && isalpha(*p)) p++;
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
print_args(NODE *node) {
  int i;
  for (i = 0; i < node->n; i++) {
    printf(" ");
    print_node(node->c[i]);
  }
}

static void
print_list(NODE *node) {
  int i;
  for (i = 0; i < node->n; i++) {
    if (i > 0) printf(" ");
    print_node(node->c[i]);
  }
}

static void
print_str(NODE *node) {
  const char* p = node->u.s;
  putchar('"');
  while (*p) {
    if (*p == '\\') putchar(*p);
    putchar(*p);
    p++;
  }
  putchar('"');
}

static void
print_node(NODE *node) {
  switch (node->t) {
  case NODE_INT: printf("%ld", node->u.i); break;
  case NODE_DOUBLE: printf("%f", node->u.d); break;
  case NODE_STRING: print_str(node); break;
  case NODE_IDENT: printf("%s", node->u.s); break;
  case NODE_NIL: printf("nil"); break;
  case NODE_PLUS: printf("(+"); print_args(node); printf(")"); break;
  case NODE_MINUS: printf("(-"); print_args(node); printf(")"); break;
  case NODE_MUL: printf("(*"); print_args(node); printf(")"); break;
  case NODE_DIV: printf("(/"); print_args(node); printf(")"); break;
  case NODE_PLUS1: printf("(1+"); print_args(node); printf(")"); break;
  case NODE_MINUS1: printf("(1-"); print_args(node); printf(")"); break;
  case NODE_IF: printf("(if"); print_args(node); printf(")"); break;
  case NODE_GT: printf("(>"); print_args(node); printf(")"); break;
  case NODE_GE: printf("(>="); print_args(node); printf(")"); break;
  case NODE_LT: printf("(<"); print_args(node); printf(")"); break;
  case NODE_LE: printf("(<="); print_args(node); printf(")"); break;
  case NODE_PRINT: printf("(print"); print_args(node); printf(")"); break;
  case NODE_QUOTE: printf("'"); print_node(node->c[0]); break;
  case NODE_LIST: printf("("); print_list(node); printf(")"); break;
  case NODE_SETQ: printf("(setq"); print_args(node); printf(")"); break;
  case NODE_DEFUN: printf("(defun"); print_args(node); printf(")"); break;
  case NODE_PROGN: print_list(node); break;
  case NODE_CALL: printf("(%s", node->u.s); print_args(node); printf(")"); break;
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
  case NODE_INT: return node->u.i; break;
  case NODE_DOUBLE: return (long)node->u.d; break;
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
      NODE *c = env->lv[i]->v;
      c->r++;
      return c;
    }
  }
  if (env->p) look_ident(env->p, k);
  /* TODO: error */
  return new_node();
}

static NODE*
look_func(ENV *env, const char *k) {
  while (env->p) env = env->p;
  int i;
  for (i = 0; i < env->nf; i++) {
    if (!strcmp(env->lf[i]->k, k)) {
      NODE *c = env->lf[i]->v;
      c->r++;
      return c;
    }
  }
  /* TODO: error */
  return NULL;
}

static NODE*
eval_node(ENV *env, NODE *node) {
  ENV *newenv;
  NODE *nn, *c, *x;
  ITEM *ni;
  int i, r;
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
    c = eval_node(env, node->c[0]);
    switch (c->t) {
    case NODE_INT: c->u.i += 1; break;
    case NODE_DOUBLE: c->u.d += 1.0; break;
    default: break;
    }
    return c;
  case NODE_MINUS1:
    c = eval_node(env, node->c[0]);
    switch (c->t) {
    case NODE_INT: c->u.i -= 1; break;
    case NODE_DOUBLE: c->u.d -= 1.0; break;
    default: break;
    }
    return c;
  case NODE_IF:
    c = eval_node(env, node->c[0]);
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
  case NODE_PRINT:
    c = eval_node(env, node->c[0]);
    print_node(c);
    puts("");
    c->r++;
    return c;
  case NODE_QUOTE:
    node->r++;
    return node;
  case NODE_SETQ:
    x = node->c[0];
    if (x->t != NODE_IDENT) {
      /* TODO: error */
      return new_node();
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
      /* TODO: error */
      puts("error");
      return new_node();
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
    c = eval_node(newenv, x->c[2]);
    free_env(newenv);
    return c;
  case NODE_DEFUN:
    x = node->c[0];
    if (x->t != NODE_IDENT) {
      /* TODO: error */
      return new_node();
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
  }
  return new_node();
}

int
main(int argc, char* argv[]) {
  ENV *env;
  NODE *top, *ret;
  char buf[BUFSIZ], *p;

  if (argc > 1) {
    FILE *fp = fopen(argv[1], "rb");
    long fsize;

    if (!fp) {
      perror("error");
      exit(1);
    }
    fseek(fp, 0, SEEK_END);
    fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);  //same as rewind(f);
    p = (char*) malloc(fsize + 1);
    memset(p, 0, fsize+1);
    fread(p, fsize, 1, fp);
    fclose(fp);

    top = new_node();
    top->t = NODE_PROGN;
    if (!parse_args(top, p)) {
      exit(1);
    }
    free(p);
    env = new_env(NULL);
    ret = eval_node(env, top);
    print_node(ret);
    puts("");
    free_node(ret);
    free_node(top);
    free_env(env);
    exit(0);
  }

  env = new_env(NULL);
  while (1) {
    printf("> ");
    if (!fgets(buf , sizeof(buf), stdin)) break;
    top = new_node();
    if (!parse_any(top, buf, 0)) {
      continue;
    }
    ret = eval_node(env, top);
    print_node(ret);
    puts("");
    free_node(ret);
    free_node(top);
  }
  free_env(env);
  return 0;
}
