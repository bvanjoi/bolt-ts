// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/classTypeParametersInStatics.ts`, Apache-2.0 License

namespace Editor {


    export class List<T> {
        public next: List<T>;
        //~^ ERROR: Property 'next' has no initializer and is not definitely assigned in the constructor.
        public prev: List<T>;
        //~^ ERROR: Property 'prev' has no initializer and is not definitely assigned in the constructor.

        constructor(public isHead: boolean, public data: T) {
        
        }

        public static MakeHead(): List<T> { // should error
          //~^ ERROR: Static members cannot reference class type parameters.
            var entry: List<T> = new List<T>(true, null);
          //~^ ERROR: Static members cannot reference class type parameters.
          //~| ERROR: Static members cannot reference class type parameters.
            entry.prev = entry;
            entry.next = entry;
            return entry;
        }        

        public static MakeHead2<T>(): List<T> { // should not error
            var entry: List<T> = new List<T>(true, null);
            //~^ ERROR: Argument of type 'null' is not assignable to parameter of type 'T'.
            entry.prev = entry;
            entry.next = entry;
            return entry;
        }  

        public static MakeHead3<U>(): List<U> { // should not error
            var entry: List<U> = new List<U>(true, null);
            //~^ ERROR: Argument of type 'null' is not assignable to parameter of type 'U'.
            entry.prev = entry;
            entry.next = entry;
            return entry;
        }  
    }
}
