// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleAndInterfaceWithSameName.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace Foo1 {
    export namespace Bar {
        export var x = 42;
    }

    export interface Bar { 
        y: string;
    }
}

namespace Foo2 {
    namespace Bar {
        export var x = 42;
    }

    export interface Bar {
        y: string;
    }
}

var z2 = Foo2.Bar.y; // Error for using interface name as a value.
//~^ ERROR: Property 'Bar' does not exist on type 'typeof Foo2'.

namespace Foo3 {
    export namespace Bar {
        export var x = 42;
    }

    interface Bar { 
        y: string;
    }
}