#ifndef _CISP_H_
#define _CISP_H_

#ifdef _MSC_VER
# define INLINE
#else
# define INLINE inline
#endif

#define UNUSED(x) (void)(x)

#define node_isnull(x) (!x || x->t == NODE_NIL)

enum NODE_TYPE {
  NODE_NIL, NODE_T, NODE_INT, NODE_DOUBLE, NODE_STRING, NODE_QUOTE, NODE_BQUOTE, NODE_IDENT,
  NODE_LAMBDA, NODE_CELL, NODE_AREF, NODE_ENV, NODE_ERROR
};

typedef struct _ENV ENV;
typedef struct _NODE NODE;

typedef NODE* (*f_do)(ENV*, NODE*);

struct _NODE {
  enum NODE_TYPE t;
  union {
    long i;
    double d;
    char* s;
    void* p;
    struct {
      NODE *car;
      NODE *cdr;
    };
  };
  f_do f;
  int r;
};

typedef struct {
  const char *k;
  NODE *v;
} ITEM;

struct _ENV {
  int nv;
  ITEM **lv;
  int nf;
  ITEM **lf;
  int nm;
  ITEM **lm;
  ENV *p;
  int r;
};

typedef struct _SCANNER SCANNER;

typedef int (*f_getc)(SCANNER*);
typedef int (*f_peek)(SCANNER*);
typedef int (*f_eof)(SCANNER*);
typedef long (*f_pos)(SCANNER*);
typedef int (*f_reset)(SCANNER*);

struct _SCANNER {
  void *v, *o;
  f_peek  _peek;
  f_getc  _getc;
  f_eof   _eof;
  f_pos   _pos;
  f_reset _reset;
  char *err;
};

typedef struct _BUFFER {
  char *ptr;
  size_t pos;
  size_t len;
} BUFFER;

NODE* new_node();
void free_node(NODE *node);
ENV* new_env(ENV *p);
void free_env(ENV *env);

void add_variable(ENV *env, const char *k, NODE *node);
void add_function(ENV *env, const char *k, NODE *node);
void add_macro(ENV *env, const char *k, NODE *node);

int node_narg(NODE *node);
NODE* eval_node(ENV *env, NODE *node);
NODE* load_lisp(ENV *env, const char *fname);
void fatal(const char *msg);

#endif /* _CISP_H_ */
