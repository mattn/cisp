#ifndef cisp_h
#define cisp_h

#ifdef _MSC_VER
# define INLINE
#else
# define INLINE inline
#endif

#ifdef _WIN32
# ifdef CISP_MAIN
#  define EXPORT __declspec(dllexport)
# else
#  define EXPORT __declspec(dllimport)
# endif
#else
# define EXPORT
#endif

#define UNUSED(x) (void)(x)

#define node_isnull(x) (!x || x->t == NODE_NIL)

typedef enum _NODE_TYPE {
  NODE_NIL, NODE_T, NODE_INT, NODE_DOUBLE, NODE_CHARACTER, NODE_STRING, NODE_QUOTE, NODE_BQUOTE, NODE_IDENT,
  NODE_LAMBDA, NODE_SPECIAL, NODE_BUILTINFUNC, NODE_CELL, NODE_AREF, NODE_ENV, NODE_ERROR,
} NODE_TYPE;

typedef enum _PRINT_MODE {
  PRINT_DEFAULT, PRINT_QUOTED,
} PRINT_MODE;

typedef struct _ENV ENV;
typedef struct _NODE NODE;

typedef NODE* (*f_do)(ENV*, NODE*);

struct _NODE {
  NODE_TYPE t;
  int r;
  union {
    wchar_t c;
    long i;
    double d;
    struct {
      char* s;
      f_do f;
    };
    struct {
      ENV *p;
      char *name;
    };
    struct {
      NODE *car;
      NODE *cdr;
    };
  };
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

EXPORT NODE* new_node();
EXPORT void free_node(NODE *node);
EXPORT ENV* new_env(ENV *p);
EXPORT void free_env(ENV *env);

EXPORT void print_node(BUFFER *buf, NODE *node, PRINT_MODE mode);

EXPORT void add_variable(ENV *env, const char *k, NODE *node);
EXPORT void add_function(ENV *env, const char *k, NODE *node);
EXPORT void add_macro(ENV *env, const char *k, NODE *node);
EXPORT void add_sym(ENV *env, NODE_TYPE t, const char* n, f_do f);
EXPORT void sort_syms(ENV *env);

EXPORT int node_narg(NODE *node);
EXPORT NODE* eval_node(ENV *env, NODE *node);
EXPORT NODE* load_lisp(ENV *env, const char *fname);
EXPORT void fatal(const char *msg);

#endif /* cisp_h */

/* vim:set et sw=2 cino=>2,\:0: */
