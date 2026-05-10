// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constDeclarations-ambient-errors.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@compiler-options: strict=false

// error: no intialization expected in ambient declarations
declare const c1: boolean = true;
//~^ ERROR: Initializers are not allowed in ambient contexts.
declare const c2: number = 0;
//~^ ERROR: Initializers are not allowed in ambient contexts.
declare const c3 = null, c4 :string = "", c5: any = 0;
//~^ ERROR: Initializers are not allowed in ambient contexts.
//~| ERROR: Initializers are not allowed in ambient contexts.
//~| ERROR: Initializers are not allowed in ambient contexts.

declare namespace M {
    const c6 = 0;
    const c7: number = 7;
  //~^ ERROR: Initializers are not allowed in ambient contexts.
}