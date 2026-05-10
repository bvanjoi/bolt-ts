// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericSignatureIdentity.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var x: {
    <T extends Date>(x: T): T;
};

var x: {
  //~^ ERROR: Subsequent variable declarations must have the same type. Variable 'x' must be of type '(x: T) => T', but here has type '(x: T) => T'.
    <T extends number>(x: T): T;
};

var x: {
  //~^ ERROR: Subsequent variable declarations must have the same type. Variable 'x' must be of type '(x: T) => T', but here has type '(x: T) => T'.
    <T>(x: T): T;
};

var x: {
  //~^ ERROR: Subsequent variable declarations must have the same type. Variable 'x' must be of type '(x: T) => T', but here has type '(x: any) => any'.
    <T>(x: any): any;
};
