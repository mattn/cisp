/*
 * Windows:
 *   gcc -fPIC -I. -shared -o lib/hello.so ./example/hello.c libcisp.a
 * Linux:
 *   gcc -fPIC -I. -shared -o lib/hello.so ./example/hello.c
 */
#include <stdio.h>
#include "cisp.h"
#include "util.h"

static NODE*
do_hello(ENV *env, NODE *alist) {
  BUFFER buf;
  buf_init(&buf);
  buf_append(&buf, "world!");
  if (node_narg(alist) > 0) {
    buf_append(&buf, ": ");
    print_node(&buf, alist->car, PRINT_DEFAULT);
  }
  puts(buf.ptr);
  buf_free(&buf);
  return new_node();
}

int
cisp_init(ENV *env) {
  NODE* c = new_node();
  add_sym(env, "hello", do_hello);
  return 0;
}
