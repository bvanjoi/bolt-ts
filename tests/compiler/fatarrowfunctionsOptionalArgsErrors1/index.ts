// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/fatarrowfunctionsOptionalArgsErrors1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

(arg1?, arg2) => 101;
//~^ ERROR: A required parameter cannot follow an optional parameter.
(...arg?) => 102;
//~^ ERROR: A rest parameter cannot be optional.
(...arg) => 103;
(...arg:number [] = []) => 104;
//~^ ERROR: A rest parameter cannot have an initializer.
// Uninitialized parameter makes the initialized one required
(arg1 = 1, arg2) => 1; 