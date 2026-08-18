// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileWithExtendsClauseThatHasItsContainerNameConflict.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

declare namespace A.B.C {
    class B {
    }
}

namespace A.B {
    export class EventManager {
        id: number;
        //~^ ERROR: Property 'id' has no initializer and is not definitely assigned in the constructor.
    }
}

namespace A.B.C {
    export class ContextMenu extends EventManager {
        name: string;
        //~^ ERROR: Property 'name' has no initializer and is not definitely assigned in the constructor.
    }
}