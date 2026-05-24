// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/forIn2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

for (var i in 1) {
  //~^ ERROR: The right-hand side of a 'for...in' statement must be of type 'any', an object type or a type parameter, but here has type '1'.
}