// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionArgShadowing.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type A = {
  a: (x: number) => string
};
type B = {
  a: (x: boolean) => string
};

function call0(p: A | B) { 
  p.a("s"); // Error
  //~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'never'.
}

function callN<T extends A | B>(p: T) {
  p.a("s"); // Error
  //~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'never'.
  
  var a: T["a"] = p.a;
  a(""); // Error
  //~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'never'.
  a("", "", "", ""); // Error
  //~^ ERROR: Expected 1 arguments, but got 4.
}