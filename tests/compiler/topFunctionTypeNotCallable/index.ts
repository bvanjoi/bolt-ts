// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/topFunctionTypeNotCallable.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

declare let foo: (...args: never) => void;
foo(); // error
//~^ ERROR: Argument of type '[]' is not assignable to parameter of type 'never'.
