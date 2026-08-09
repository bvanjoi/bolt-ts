// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/conditionalTypeDoesntSpinForever.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

// Via #56620
class Base<U> { }
export class C2<T> extends Base<unknown> {
    T: number;
    constructor(T: number) {
        super();
        // Should not error
        this.T = T;

        // Should error
        let a: U = null;
        //~^ ERROR: Cannot find name 'U'.
    }
}

// via #56689
class Leg { }
class Foo<t> extends Leg {
    t = {} as t

    // should allow this access since t was declared as a property on Foo
    foo = this.t
}

// via #56661
class BaseClass { }
class Item<data> extends BaseClass {
    data: data;
    //~^ ERROR: Property 'data' has no initializer and is not definitely assigned in the constructor.
    getData() {
        // should OK
        return this.data;
    }
}