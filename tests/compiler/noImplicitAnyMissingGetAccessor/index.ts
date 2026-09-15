// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyMissingGetAccessor.ts`, Apache-2.0 License

//@compiler-options: noImplicitAny
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

abstract class Parent
{
    public abstract set message(str);
    //~^ ERROR: Property 'message' implicitly has type 'any', because its set accessor lacks a parameter type annotation.
    //~| ERROR: Parameter 'str' implicitly has an 'any' type.
}

class Child extends Parent {
    _x: any;
    public set message(str) {
    //~^ ERROR: Property 'message' implicitly has type 'any', because its set accessor lacks a parameter type annotation.
    //~| ERROR: Parameter 'str' implicitly has an 'any' type.
      this._x = str;
    }
}