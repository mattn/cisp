# cisp

[![CI](https://github.com/mattn/cisp/workflows/CI/badge.svg)](https://github.com/mattn/cisp/actions)

A compact Lisp interpreter written in C.

## Features

- **Cross-platform**: Runs on Linux, macOS, and Windows
- **Lightweight**: Single binary with minimal dependencies
- **Lisp-2**: Functions and variables have separate namespaces
- **Dynamic library support**: Load shared libraries with the `load` function
- **First-class continuations**: Supports `call/cc`
- **Tail call optimization**: Efficient execution of recursive functions
- **Macro system**: Full macro support with backquote syntax
- **Rich data types**: Integers, floats, characters, strings, symbols, and cons cells
- **C plugin support**: Write plugins in C for extending functionality

## C Plugin Example

You can extend cisp with C plugins. See `example/hello.c` for an example:

```c
int cisp_init(ENV *env) {
  add_sym(env, NODE_BUILTINFUNC, "hello", do_hello);
  return 0;
}
```

Compile with:
```sh
# Linux
gcc -fPIC -I. -shared -o lib/hello.so ./example/hello.c

# Windows
gcc -fPIC -I. -shared -o lib/hello.so ./example/hello.c libcisp.a
```

## Build

```sh
$ make
```

## License

Public Domain

## Author

Yasuhiro Matsumoto (a.k.a. mattn)
