// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/ambientErrors1.ts`, Apache-2.0 License

declare var x = 4;
//~^ ERROR: Initializers are not allowed in ambient contexts.
declare const a: number = 42;
//~^ ERROR: Initializers are not allowed in ambient contexts.
declare const b: number;
declare const c0 = 'something';
declare const c1 = false;
declare const c2 = true;
declare const c3 = 42;
