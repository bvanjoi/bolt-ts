// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionOverloadsRecursiveGenericReturnType.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class B<V>{
    private id: V;
    //~^ ERROR: Property 'id' has no initializer and is not definitely assigned in the constructor.
}

class A<U>{
    GetEnumerator: () => B<U>;
    //~^ ERROR: Property 'GetEnumerator' has no initializer and is not definitely assigned in the constructor.
}

function Choice<T>(args: T[]): A<T>;
function Choice<T>(...v_args: T[]): A<T>;
function Choice<T>(...v_args: any[]): A<T>{
    return new A<T>();
}
