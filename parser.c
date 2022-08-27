#define _CRT_SECURE_NO_WARNINGS 1
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#ifndef _MSC_VER
#include <unistd.h>
#else
# include <io.h>
# define isatty(f) _isatty(f)
# define fileno(f) _fileno(f)
# define strdup(x) _strdup(x)
# define snprintf(b,n,f,...) _snprintf(b,n,f,__VA_ARGS__)
#endif

#define CISP_MAIN

#include "cisp.h"
#include "parser.h"

#define SYMBOL_CHARS "+-*/<>=&%?.@_#$:*"

static NODE*
raise(SCANNER *s, const char *p) {
  s->err = strdup(p);
  return NULL;
}

void
fatal(const char *msg) {
  fputs(msg, stderr);
  exit(1);
}

NODE*
invalid_token(SCANNER *s) {
  char buf[BUFSIZ], c;
  size_t i, l, o;
  long pos = s_pos(s);
  snprintf(buf, sizeof(buf)-1, "invalid token at offset %ld", pos == -1 ? 0 : pos);
  l = strlen(buf);

  s_reset(s);
  if (!isatty(fileno(stdin))) {
    buf[l++] = '\n';
    o = l;
    for (i = 0; l < sizeof(buf)-1; i++) {
      c = s_getc(s);
      if (s_eof(s)) break;
      if (c == '\n') {
        if (i >= (size_t)pos) break;
        l = o;
        continue;
      }
      buf[l++] = c;
    }
    buf[l] = 0;
  }
  s->err = strdup(buf);
  return NULL;
}

int
s_peek(SCANNER *s) {
  return s->_peek(s);
}

int
s_getc(SCANNER *s) {
  return s->_getc(s);
}

int
s_eof(SCANNER *s) {
  return s->_eof(s);
}

long
s_pos(SCANNER *s) {
  return s->_pos(s);
}

int
s_reset(SCANNER *s) {
  int r = s->_reset(s);
  if (s->err) free(s->err);
  s->err = NULL;
  return r;
}

static int
file_peek(SCANNER *s) {
  int c = fgetc((FILE*)s->v);
  if (c == -1) return c;
  ungetc(c, (FILE*)s->v);
  return c;
}

static int
file_getc(SCANNER *s) {
  return fgetc((FILE*)s->v);
}

static int
file_eof(SCANNER *s) {
  return feof((FILE*)s->v);
}

static long
file_pos(SCANNER *s) {
  return ftell((FILE*)s->v);
}

static int
file_reset(SCANNER *s) {
  return fseek((FILE*)s->v, 0, SEEK_SET);
}

void
s_file_init(SCANNER *s, FILE* v) {
  s->v = (void*)v;
  s->o = (void*)v;
  s->_peek  = file_peek;
  s->_getc  = file_getc;
  s->_eof   = file_eof;
  s->_pos   = file_pos;
  s->_reset = file_reset;
  s->err   = NULL;
}

#if 0
static int
string_peek(SCANNER *s) {
  return *((char*)s->v);
}

static int
string_getc(SCANNER *s) {
  int c = *((char*)s->v);
  s->v = ((char*)s->v) + 1;
  return c;
}

static int
string_eof(SCANNER *s) {
  return *((char*)s->v) == 0;
}

static long
string_pos(SCANNER *s) {
  return (long)((uintptr_t)s->v - (uintptr_t)s->o);
}

static int
string_reset(SCANNER *s) {
  return 0;
}

static void
s_string_init(SCANNER *s, char* v) {
  s->v = (void*)v;
  s->o = (void*)v;
  s->_peek  = string_peek;
  s->_getc  = string_getc;
  s->_eof   = string_eof;
  s->_pos   = string_pos;
  s->_reset = string_reset;
  s->err   = NULL;
}
#endif

static INLINE int
match(const char *lhs, const char *rhs, size_t n) {
  const char *p = lhs, *e = lhs + n;
  while (p < e) if (!*rhs || *p++ != *rhs++) return 0;
  if (*rhs) return 0;
  return 1;
}

NODE*
parse_paren(SCANNER *s, int mode) {
  NODE *head, *node, *x;

  skip_white(s);
  if (s_eof(s)) return raise(s, "unexpected end of file");

  head = node = new_node();
  node->t = NODE_CELL;
  while (!s_eof(s) && s_peek(s) != ')') {
    NODE *child;
    char q = s_peek(s) == ',';
    if (q) s_getc(s);

    child = parse_any(s, PARSE_ANY);
    if (child == NULL) return NULL;

    if ((mode & PARSE_BQUOTE) != 0 && !q) {
      NODE *r = new_node();
      r->t = NODE_QUOTE;
      r->car = child;
      child = r;
    }

    if (child->t == NODE_IDENT && !strcmp(".", child->s)) {
      if (!head->car) {
        free_node(child);
        return raise(s, "illegal dot operation");
      }
      free_node(child);

      child = parse_any(s, PARSE_ANY);
      if (child == NULL) return NULL;
      node->cdr = child;
      break;
    } else {
      if (head->car) {
        x = new_node();
        x->t = NODE_CELL;
        node->cdr = x;
        node = x;
      }
      node->car = child;
    }

    skip_white(s);
  }

  if (!head->car && !head->cdr)
    head->t = NODE_NIL;

  return head;
}

static NODE*
parse_primitive(SCANNER *s) {
  char buf[BUFSIZ];
  size_t n = 0;
  char *e;
  int c;
  NODE *x;

  while (n < sizeof(buf) && !s_eof(s)) {
    c = s_peek(s);
    if (c == -1) return NULL;
    if (isalnum(c) || strchr(SYMBOL_CHARS, c)) buf[n++] = s_getc(s);
    else break;
  }
  buf[n] = 0;

  x = new_node();
  if (match(buf, "nil", n)) {
    return x;
  }
  if (match(buf, "t", n)) {
    x->t = NODE_T;
    return x;
  }
  x->i = strtol(buf, &e, 10);
  if (e == buf+n) {
    x->t = NODE_INT;
    return x;
  }
  x->d = strtod(buf, &e);
  if (e == buf+n) {
    x->t = NODE_DOUBLE;
    return x;
  }
  x->t = NODE_IDENT;
  x->s = (char*)malloc(n + 1);
  memset(x->s, 0, n + 1);
  memcpy(x->s, buf, n);
  return x;
}

static NODE*
parse_quote(SCANNER *s) {
  NODE *node, *child;

  s_getc(s);
  child = parse_any(s, PARSE_ANY);
  if (child == NULL) return NULL;
  node = new_node();
  node->t = NODE_QUOTE;
  node->car = child;
  return node;
}

static NODE*
parse_bquote(SCANNER *s) {
  NODE *node, *child;

  s_getc(s);
  child = parse_any(s, PARSE_BQUOTE);
  if (child == NULL) return NULL;
  node = new_node();
  node->t = NODE_BQUOTE;
  node->car = child;
  return node;
}

static NODE*
parse_string(SCANNER *s) {
  char *buf = NULL;
  int n = 0, l = 0;
  int c = 0;
  NODE *node;

  buf = (char*)malloc(10);
  s_getc(s);
  while (!s_eof(s)) {
    c = s_getc(s);
    if (c == '\\' && !s_eof(s)) {
      c = s_getc(s);
      switch (c) {
      case '\\': c = '\\'; break;
      case 'b': c = '\b'; break;
      case 'f': c = '\f'; break;
      case 'n': c = '\n'; break;
      case 'r': c = '\r'; break;
      case 't': c = '\t'; break;
      default: free(buf); return invalid_token(s);
      }
    } else if (c == '"') break;
    if (n >= l-1) {
      buf = (char*)realloc(buf, l+40);
      l += 40;
    }
    buf[n++] = c;
  }
  buf[n] = 0;
  if (c != '"') {
    free(buf);
    return invalid_token(s);
  }

  node = new_node();
  node->t = NODE_STRING;
  node->s = buf;
  return node;
}

NODE*
parse_any(SCANNER *s, int mode) {
  NODE *x = NULL;
  int c;

  skip_white(s);
  if (s_eof(s)) return raise(s, "unexpected end of file");

  c = s_peek(s);

  if (c == '(') {
    s_getc(s);
    x = parse_paren(s, mode);
    if (x == NULL) return NULL;
    if (s_eof(s)) {
      return raise(s, "unexpected end of file");
    }
    skip_white(s);
    if (s_getc(s) != ')') {
      return invalid_token(s);
    }
  } else if (c == '\'')
    x = parse_quote(s);
  else if (c == '`')
    return parse_bquote(s);
  else if (c == '"')
    x = parse_string(s);
  else if (isalnum((int)c) || strchr(SYMBOL_CHARS, c))
    x = parse_primitive(s);
  else
    return invalid_token(s);

  return x;
}

/* vim:set et sw=2 cino=>2,\:0: */
