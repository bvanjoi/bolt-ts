// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/ambientStatement1.ts`, Apache-2.0 License

declare module M1 {
  while(true);
  //~^ ERROR: Statements are not allowed in ambient contexts.

  export var v1 = () => false;
  //~^ ERROR: Initializers are not allowed in ambient contexts.
}