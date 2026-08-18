// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/clodulesDerivedClasses.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Shape {
    id: number;
    //~^ ERROR: Property 'id' has no initializer and is not definitely assigned in the constructor.
}

namespace Shape.Utils {
    export function convert(): Shape { return null;}
    //~^ ERROR: Type 'null' is not assignable to type 'Shape'.
}

class Path extends Shape {
  //~^ ERROR: Property 'convert' is missing.
    name: string;
    //~^ ERROR: Property 'name' has no initializer and is not definitely assigned in the constructor.

}

namespace Path.Utils {
    export function convert2(): Path {
        return null;
        //~^ ERROR: Type 'null' is not assignable to type 'Path'.
    }
}