// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericClassWithStaticsUsingTypeArguments.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

// Should be error to use 'T' in all declarations within Foo.
class Foo<T> {
    static a = (n: T) => { };
    //~^ ERROR: Static members cannot reference class type parameters.
    
    static b: T;
    //~^ ERROR: Static members cannot reference class type parameters.

    static c: T[] = [];
    //~^ ERROR: Static members cannot reference class type parameters.

    static d = false || ((x: T) => x || undefined)(null)
    //~^ ERROR: Static members cannot reference class type parameters.

    static e = function (x: T) { return null; }
    //~^ ERROR: Static members cannot reference class type parameters.

    static f(xs: T[]): T[] {
    //~^ ERROR: Static members cannot reference class type parameters.
    //~| ERROR: Static members cannot reference class type parameters.
        return xs.reverse();
    }
}

