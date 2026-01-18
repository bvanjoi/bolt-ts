// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/nullableFunctionError.ts`, Apache-2.0 License

//@compiler-options: strictNullChecks

null();
//~^ ERROR: Cannot invoke an object which is possibly 'null'.
undefined();
//~^ ERROR: Cannot invoke an object which is possibly 'undefined'.
let f: null | undefined;
f();
//~^ ERROR: Cannot invoke an object which is possibly 'null' or 'undefined'.
