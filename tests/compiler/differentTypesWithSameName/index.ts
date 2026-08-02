// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/differentTypesWithSameName.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace m {
  export class variable{
    s: string;
    //~^ ERROR: Property 's' has no initializer and is not definitely assigned in the constructor.
  }
  export function doSomething(v: m.variable) {
    
  }
}

class variable {
 t: number;
 //~^ ERROR: Property 't' has no initializer and is not definitely assigned in the constructor.
}


var v: variable = new variable();
m.doSomething(v);
 //~^ ERROR: Property 's' is missing.
