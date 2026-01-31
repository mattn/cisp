# cisp

[![CI](https://github.com/mattn/cisp/workflows/CI/badge.svg)](https://github.com/mattn/cisp/actions)

A compact Lisp interpreter written in C.

## Features

- **Cross-platform**: Runs on Linux, macOS, and Windows
- **Lightweight**: Single binary with minimal dependencies
- **Lisp-1**: Functions and variables share the same namespace
- **Dynamic library support**: Load shared libraries with the `load` function
- **First-class continuations**: Supports `call/cc`
- **Tail call optimization**: Efficient execution of recursive functions
- **Macro system**: Full macro support with backquote syntax
- **Rich data types**: Integers, floats, characters, strings, symbols, and cons cells

## Build

```sh
$ make
```

## License

Public Domain

## Author

Yasuhiro Matsumoto (a.k.a. mattn)
