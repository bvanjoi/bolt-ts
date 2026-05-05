// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mergedDeclarations3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
namespace M {
 export enum Color {
   Red, Green
 }
}
namespace M {
 export namespace Color {
   export var Blue = 4;
  }
}
var p = M.Color.Blue; // ok

namespace M {
    export function foo() {
    }
}

namespace M {
    namespace foo {
        export var x = 1;
    }
}

namespace M {
    export namespace foo {
        export var y = 2
    }
}

namespace M {
    namespace foo {
        export var z = 1;
    }
}

M.foo() // ok
M.foo.x // error
//~^ ERROR: Property 'x' does not exist on type 'typeof foo'.
M.foo.y // ok
M.foo.z // error
//~^ ERROR: Property 'z' does not exist on type 'typeof foo'.
