#define _CRT_SECURE_NO_WARNINGS 1
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <sys/stat.h>
#ifdef _WIN32
# include <windows.h>
#endif
#ifndef _MSC_VER
# include <unistd.h>
# include <dirent.h>
#else
# include <direct.h>
# define strdup(x) _strdup(x)
# define isatty(f) _isatty(f)
# define fileno(f) _fileno(f)
# define snprintf(b,n,f,...) _snprintf(b,n,f,__VA_ARGS__)
# define PATH_MAX MAX_PATH
#endif

#include "cisp.h"
#include "util.h"

static void
walk(ENV *env, char *base) {
  char path[PATH_MAX];
  DIR *dir;
  struct dirent *ent;
  struct stat st;

  dir = opendir(base);
  if (!dir) return ;

  ent = readdir(dir);
  while (ent) {
    if (*(ent->d_name) != '.') {
      snprintf(path, sizeof(path)-1, "%s/%s", base, ent->d_name);
      if (stat(path, &st)) {
        fprintf(stderr, "failed to get stat %s\n", path);
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
        }
      }
    }
    ent = readdir(dir);
  }
  closedir(dir);
}

void
load_libs(ENV *env) {
  char path[PATH_MAX], *top = path, *ptr;

#if defined(__APPLE__)
  uint32_t size = sizeof(path);
  if (_NSGetExecutablePath(path, &size) == 0)
    fatal("cound't get module information");
#elif defined(__linux__)
  ssize_t len = readlink("/proc/self/exe", path, sizeof(path)-1);
  if (len == -1)
    fatal("cound't get module information");
  path[len] = '\0';
#elif defined(_WIN32)
  if (GetModuleFileName(NULL, path, sizeof(path)) == 0)
    fatal("cound't get module information");
  ptr = path;
  while (*ptr) { if (*ptr == '\\') *ptr = '/'; ptr++; }
#else
  fatal("cound't get module information");
#endif
  ptr = top + strlen(top) - 1;
  while (*ptr != '/') ptr--;
  *ptr = 0;
  ptr = top + strlen(top) - 4;
  if (strcmp("/bin", ptr))
    ptr += 4;
  strcpy(ptr, "/lib");

  walk(env, path);
}
