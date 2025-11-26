// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/qualifiedModuleLocals.ts`, Apache-2.0 License

namespace A {

  function b() {}

  export function a(){  A.b();  } // A.b should be an unresolved symbol error
  //~^ ERROR: Property 'b' does not exist on type 'typeof A'.

}

A.a();
