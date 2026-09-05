// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unaryOperatorsInStrictMode.ts`, Apache-2.0 License

//@compiler-options: target=es2015

"use strict"

++eval;
//~^ ERROR: Invalid use of 'eval' in strict mode.
//~| ERROR: Cannot assign to 'eval' because it is a function.
--eval;
//~^ ERROR: Invalid use of 'eval' in strict mode.
//~| ERROR: Cannot assign to 'eval' because it is a function.
++arguments;
//~^ ERROR: Invalid use of 'arguments' in strict mode.
//~| ERROR: Cannot find name 'arguments'.
--arguments;
//~^ ERROR: Invalid use of 'arguments' in strict mode.
//~| ERROR: Cannot find name 'arguments'.
eval++;
//~^ ERROR: Invalid use of 'eval' in strict mode.
//~| ERROR: Cannot assign to 'eval' because it is a function.
eval--;
//~^ ERROR: Invalid use of 'eval' in strict mode.
//~| ERROR: Cannot assign to 'eval' because it is a function.
arguments++;
//~^ ERROR: Invalid use of 'arguments' in strict mode.
//~| ERROR: Cannot find name 'arguments'.
arguments--;
//~^ ERROR: Invalid use of 'arguments' in strict mode.
//~| ERROR: Cannot find name 'arguments'.
