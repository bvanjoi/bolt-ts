// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyNamelessParameter.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

class C { }
declare var a: { m(...string): void }
//~^ ERROR: Rest parameter 'string' implicitly has an 'any[]' type.
declare var b: (string, C) => void;
//~^ ERROR: Parameter 'string' implicitly has an 'any' type.
//~| ERROR: Parameter 'C' implicitly has an 'any' type.
declare var c: { (C, number): void };
//~^ ERROR: Parameter 'number' implicitly has an 'any' type.
//~| ERROR: Parameter 'C' implicitly has an 'any' type.
declare var d: { m(boolean, C, object, undefined): void }
//~^ ERROR: Parameter 'boolean' implicitly has an 'any' type.
//~| ERROR: Parameter 'C' implicitly has an 'any' type.
//~| ERROR: Parameter 'object' implicitly has an 'any' type.
//~| ERROR: Parameter 'undefined' implicitly has an 'any' type.
// note: null and void do not parse correctly without a preceding parameter name
