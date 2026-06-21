// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextuallyTypedParametersWithQuestionToken.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

// https://github.com/microsoft/TypeScript/issues/54948

function acceptNum(num: number) {}

const f1: (a: string, b: number) => void = function self(a, b?) {
  acceptNum(b); // error
  //~^ ERROR: Argument of type 'undefined | number' is not assignable to parameter of type 'number'.
  self("");
  self("", undefined);
};

const f2: (a: string, b: number) => void = function self(a, b?: number) {
  acceptNum(b); // error
  //~^ ERROR: Argument of type 'undefined | number' is not assignable to parameter of type 'number'.
  self("");
  self("", undefined);
};
