// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nonExportedElementsOfMergedModules.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

module One {
  enum A { X }
  module B {
      export var x;
  }
}

module One {
  enum A { Y }
  module B {
      export var y;
  }
  B.x;
  //~^ Error: Property 'x' does not exist on type 'typeof B'.
  B.y;
}


interface A {
  a0: number
}

interface A {
  a1: string
}

function f(a: A) {
  let b0: string = a.a0;
  //~^ ERROR: Type 'number' is not assignable to type 'string'.
  let b1: number = a.a1;
  //~^ ERROR: Type 'string' is not assignable to type 'number'.
}
