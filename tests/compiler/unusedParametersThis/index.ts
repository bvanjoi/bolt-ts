// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedParametersThis.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitThis
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

class A {
    public a: number;
    //~^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.

    public method(this: this): number {
        return this.a;
    }

    public method2(this: A): number {
        return this.a;
    }

    public method3(this: this): number {
        var fn = () => this.a;
        return fn();
    }

    public method4(this: A): number {
        var fn = () => this.a;
        return fn();
    }

    static staticMethod(this: A): number {
        return this.a;
    }
}

function f(this: A): number {
    return this.a
}

var f2 = function f2(this: A): number {
    return this.a;
};