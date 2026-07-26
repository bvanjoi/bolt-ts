// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/extendAndImplementTheSameBaseType2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C<T> {
    foo: number
    //~^ ERROR: Property 'foo' has no initializer and is not definitely assigned in the constructor.
    bar(): T {
        return null;
        //~^ ERROR: Type 'null' is not assignable to type 'T'.
    }
}
class D extends C<string> implements C<number> {
  //~^ ERROR: Class 'D' incorrectly implements interface 'C'.
    baz() { }
}

var d: D = new D();
var r: string = d.foo;
//~^ ERROR: Type 'number' is not assignable to type 'string'.
var r2: number = d.foo;

var r3: string = d.bar();
var r4: number = d.bar();
//~^ ERROR: Type 'string' is not assignable to type 'number'.
