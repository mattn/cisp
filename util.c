#define _CRT_SECURE_NO_WARNINGS 1
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#ifdef _WIN32
#include <windows.h>
#endif
#ifndef _MSC_VER
#include <dirent.h>
#include <dlfcn.h>
#include <unistd.h>
#else
#include <direct.h>
#include <io.h>
#define strdup(x) _strdup(x)
#define isatty(f) _isatty(f)
#define fileno(f) _fileno(f)
#define snprintf(b, n, f, ...) _snprintf(b, n, f, __VA_ARGS__)
#define PATH_MAX MAX_PATH
#endif

#define CISP_MAIN

#include "cisp.h"
#include "util.h"

#ifdef _MSC_VER
struct dirent {
  char *d_name;
};

typedef struct _DIR {
  intptr_t h;
  struct _finddata_t fi;
  struct dirent ent;
  char *name;
} DIR;

static DIR *opendir(const char *name) {
  DIR *dir = NULL;
  size_t len;
  const char *mask;

  if (!name || !*name) {
    errno = EINVAL;
    return NULL;
  }
  len = strlen(name);
  mask = strchr("/\\", name[len - 1]) ? "*" : "/*";
  dir = (DIR *)malloc(sizeof *dir);
  if (!dir) {
    errno = ENOMEM;
    return NULL;
  }
  dir->name = (char *)malloc(len + strlen(mask) + 1);
  if (!dir) {
    errno = ENOMEM;
    free(dir);
    return NULL;
  }
  strcpy(dir->name, name);
  strcat(dir->name, mask);
  if ((dir->h = _findfirst(dir->name, &dir->fi)) != -1) {
    dir->ent.d_name = NULL;
  } else {
    free(dir->name);
    free(dir);
    dir = NULL;
  }
  return dir;
}

static int closedir(DIR *dir) {
  int r = -1;
  if (!dir) {
    errno = EBADF;
    return -1;
  }
  if (dir->h != -1)
    r = _findclose(dir->h);
  free(dir->name);
  free(dir);
  if (r == -1)
    errno = EBADF;
  return r;
}

struct dirent *readdir(DIR *dir) {
  struct dirent *ent = NULL;
  if (!dir || dir->h == -1) {
    errno = EBADF;
    return NULL;
  }
  if (!dir->ent.d_name || _findnext(dir->h, &dir->fi) != -1) {
    ent = &dir->ent;
    ent->d_name = dir->fi.name;
  }
  return ent;
}
#endif

static void walk(ENV *env, char *base) {
  char path[PATH_MAX + 1];
  DIR *dir;
  struct dirent *ent;
  struct stat st;
  int sym_add = 0;

  dir = opendir(base);
  if (!dir)
    return;

  ent = readdir(dir);
  while (ent) {
    if (*(ent->d_name) != '.') {
      snprintf(path, sizeof(path), "%s/%s", base, ent->d_name);
      if (stat(path, &st)) {
        fprintf(stderr, "failed to get stat: %s\n", path);
        break;
      }
      if ((st.st_mode & S_IFMT) == S_IFDIR)
        walk(env, path);
      else {
        size_t len = strlen(path);
        if (!strcmp(path + len - 5, ".lisp")) {
          NODE *ret = load_lisp(env, path);
          if (ret->t == NODE_ERROR)
            fprintf(stderr, "cisp: %s\n", ret->s);
          free_node(ret);
        } else if (!strcmp(path + len - 3, ".so")) {
#ifndef _MSC_VER
          void *handle;
          dlerror();
          handle = dlopen(path, RTLD_GLOBAL | RTLD_NOW);
          if (handle) {
            typedef int (*f_cisp_init)(ENV *);
            f_cisp_init fcn;
            *(void **)(&fcn) = dlsym(handle, "cisp_init");
            if (fcn) {
              if (fcn(env) == 0) {
                sym_add++;
              } else {
                fprintf(stderr, "failed to load library: %s\n", path);
              }
            }
          } else {
            fprintf(stderr, "failed to load library: %s\n", dlerror());
          }
#else
          fprintf(stderr, "failed to load library: %s\n", path);
#endif
        }
      }
    }
    ent = readdir(dir);
  }
  closedir(dir);
  if (sym_add)
    sort_syms(env);
}

void load_libs(ENV *env) {
  char path[PATH_MAX], *top = path, *ptr;

#if defined(__APPLE__)
  uint32_t size = sizeof(path);
  if (_NSGetExecutablePath(path, &size) != 0)
    fatal("couldn't get module information");
#elif defined(__linux__)
  ssize_t len = readlink("/proc/self/exe", path, sizeof(path) - 1);
  if (len == -1)
    fatal("couldn't get module information");
  path[len] = '\0';
#elif defined(_WIN32)
  if (GetModuleFileName(NULL, path, sizeof(path)) == 0)
    fatal("couldn't get module information");
  ptr = path;
  while (*ptr) {
    if (*ptr == '\\')
      *ptr = '/';
    ptr++;
  }
#else
  fatal("couldn't get module information");
#endif
  ptr = top + strlen(top) - 1;
  while (*ptr != '/')
    ptr--;
  *ptr = 0;
  ptr = top + strlen(top) - 4;
  if (strcmp(ptr, "/bin") != 0)
    ptr += 4;
  strcpy(ptr, "/lib");

  walk(env, path);
}

void buf_init(BUFFER *b) {
  b->ptr = NULL;
  b->len = 0;
  b->pos = 0;
}

void buf_append(BUFFER *b, const char *s) {
  size_t len = strlen(s);
  if (b->pos + len + 1 > b->len) {
    b->ptr = (char *)realloc(b->ptr, b->len + len + 100);
    *(b->ptr + b->pos) = 0;
    b->len += len + 100;
  }
  while (*s) {
    *(b->ptr + b->pos++) = *s++;
  }
  *(b->ptr + b->pos) = 0;
}

void buf_free(BUFFER *b) { free(b->ptr); }

typedef struct _INTERN_ITEM {
  const char *k;
  struct _INTERN_ITEM *next;
} INTERN_ITEM;

static INTERN_ITEM *intern_list[4099];

static unsigned int hash(const char *s) {
  unsigned int h = 0;
  while (*s)
    h = h * 31 + *s++;
  return h;
}

const char *intern(const char *s) {
  unsigned int h = hash(s) % 4099;
  INTERN_ITEM *item = intern_list[h];
  while (item) {
    if (!strcmp(item->k, s))
      return item->k;
    item = item->next;
  }
  item = (INTERN_ITEM *)malloc(sizeof(INTERN_ITEM));
  item->k = strdup(s);
  item->next = intern_list[h];
  intern_list[h] = item;
  return item->k;
}

/* vim:set et sw=2 cino=>2,\:0: */
