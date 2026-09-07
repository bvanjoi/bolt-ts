// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyMissingSetAccessor.ts`, Apache-2.0 License

//@compiler-options: noImplicitAny
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015


abstract class Parent
{
    public abstract get message();
    //~^ ERROR: Property 'message' implicitly has type 'any', because its get accessor lacks a return type annotation.
}

class Child extends Parent {
    public get message() {
        return "";
    }
}
