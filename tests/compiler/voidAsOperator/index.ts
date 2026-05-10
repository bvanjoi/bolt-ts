// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/voidAsOperator.ts`, Apache-2.0 License

//@compiler-options: target=es2015

if (!void 0 !== true) {
  //~^ ERROR: This kind of expression is always falsy.
 
}

//CHECK#2
if (!null !== true) {
  //~^ ERROR: This kind of expression is always falsy.
 
}
