#include <stdio.h>
#include <cisp.h>

static NODE*
do_hello(ENV *env, NODE *alist) {
  puts("world!");
  return new_node();
}

int
cisp_init(ENV *env) {
  NODE* c = new_node();
  add_sym(env, "hello", do_hello);
  return 0;
}
