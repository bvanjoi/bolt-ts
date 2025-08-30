// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/typeValueConflict1.ts`, Apache-2.0 License

module M1 {
 export class A {
 }
}
module M2 {
 var M1 = 0;
 // Should error.  M1 should bind to the variable, not to the module.
 class B extends M1.A {
  //~^ ERROR:  Property 'A' does not exist on type 'number'.
 }
}