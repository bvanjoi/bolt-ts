// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/circularOptionalityRemoval.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks

// Constructed repro
function fn1(x: number | undefined = x > 0 ? x : 0) { }
//~^ ERROR: Parameter 'x' cannot reference itself.
//~| ERROR: Parameter 'x' cannot reference itself.
//~| ERROR: 'x' is possibly 'undefined'.
//~| ERROR: 'x' is possibly 'undefined'.
//~| ERROR: 'x' is referenced directly or indirectly in its own type annotation.
//~| ERROR: 'x' is referenced directly or indirectly in its own type annotation.
//~| ERROR: 'x' is referenced directly or indirectly in its own type annotation.

// Report from user
function fn2(x?: string = someCondition ? 'value1' : x) { }
//~^ ERROR: Cannot find name 'someCondition'.
//~| ERROR: Parameter 'x' cannot reference itself.
//~| ERROR: Parameter cannot have question mark and initializer.
//~| ERROR: 'x' is referenced directly or indirectly in its own type annotation.
//~| ERROR: 'x' is referenced directly or indirectly in its own type annotation.

