// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/circularTypeArgumentsLocalAndOuterNoCrash1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

// https://github.com/microsoft/TypeScript/issues/59062

function f<_T, _S>() {
  interface NumArray<T extends number> extends Array<T> {}
  type X = NumArray<X extends {} ? number : number>;
  //~^ ERROR: Type arguments for 'NumArray' circularly reference themselves.
  //~| ERROR: Type arguments for 'NumArray' circularly reference themselves.
}
