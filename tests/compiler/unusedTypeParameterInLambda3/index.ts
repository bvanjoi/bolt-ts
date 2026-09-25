// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedTypeParameterInLambda3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

class A<T> {
    public x: T;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
}

var y: new <T,U>(a:T)=>void;
//~^ ERROR: 'U' is declared but its value is never read.