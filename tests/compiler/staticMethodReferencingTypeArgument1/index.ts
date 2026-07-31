// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/staticMethodReferencingTypeArgument1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace Editor {
    export class List<T> {
        next: List<T>;
        //~^ ERROR: Property 'next' has no initializer and is not definitely assigned in the constructor.
        prev: List<T>;
        //~^ ERROR: Property 'prev' has no initializer and is not definitely assigned in the constructor.

        constructor(public isHead: boolean, public data: T) {
        }

        static MakeHead(): List<T> {
          //~^ ERROR: Static members cannot reference class type parameters.
            var entry: List<T> = new List<T>(true, null); // can't access T here
          //~^ ERROR: Static members cannot reference class type parameters.
          //~| ERROR: Static members cannot reference class type parameters.
            entry.prev = entry;
            entry.next = entry;
            return entry;
        }
    }
}