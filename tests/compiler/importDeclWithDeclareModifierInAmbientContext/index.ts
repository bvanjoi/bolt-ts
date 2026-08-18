// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/importDeclWithDeclareModifierInAmbientContext.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare module "m" {
    namespace x {
        interface c {
        }
    }
    declare export import a = x.c;
    //~^ ERROR: A 'declare' modifier cannot be used with an import declaration.
    //~| ERROR: A 'declare' modifier cannot be used in an already ambient context.
    //~| ERROR: 'export' modifier must precede 'declare' modifier.
    declare const d = 1;
    //~^ ERROR: A 'declare' modifier cannot be used in an already ambient context.
    var b: a;
}

namespace v { 
  export const b = 2;
}
declare import a = v.b;
//~^ ERROR: A 'declare' modifier cannot be used with an import declaration.

function f() {
  import a = v.b;
  //~^ ERROR: An import declaration can only be used at the top level of a namespace or module.
}

namespace M {
  export namespace N0 {
    export var v = 1;
  }
  export namespace N1 {
    export var v = 1;
    export namespace N3 {
      export var v = 1;
    }
    export namespace N2.X {
      export var v = 1;
    }
  }
}