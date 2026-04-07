// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/wrappedIncovations2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

var v = this
  .foo()
  .bar()
  .baz();