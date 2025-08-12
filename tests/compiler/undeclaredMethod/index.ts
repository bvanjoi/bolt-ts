// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/undeclaredMethod.ts`, Apache-2.0 License

module M {
  export class C {
      public salt() {}
  }
}

var c:M.C = new M.C();

c.salt();	// cool
c.saltbar();	// crash
//~^ ERROR: Property 'saltbar' does not exist on type 'C'.

