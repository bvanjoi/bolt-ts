// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitExpressionInExtends.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

var x: {
    new<T>(s: any): Q;
}

class Q {
    s: string;    
    //~^ ERROR: Property 's' has no initializer and is not definitely assigned in the constructor.
}

class B extends x<string> {    
  //~^ ERROR: Variable 'x' is used before being assigned.
}

var q: B;
q.s;
  //~^ ERROR: Variable 'q' is used before being assigned.
