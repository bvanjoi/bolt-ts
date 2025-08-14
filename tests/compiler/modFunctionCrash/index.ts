// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/modFunctionCrash.ts`, Apache-2.0 License

declare module Q {
  function f(fn:()=>void); // typechecking the function type shouldnot crash the compiler
}


Q.f(function() {this;});
Q.f(function() {return 42;});
Q.f(function() {
  let a: string = 42;
  //~^ ERROR: Type 'number' is not assignable to type 'string'.
});
