// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfaceClassMerging.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Foo {
    method(a: number): string;
    optionalMethod?(a: number): string;
    property: string;
    optionalProperty?: string;
}

class Foo {
    additionalProperty: string;
    //~^ ERROR: Property 'additionalProperty' has no initializer and is not definitely assigned in the constructor.

    additionalMethod(a: number): string {
        return this.method(0);
    }
}

class Bar extends Foo {
    method(a: number) {
      //~^ ERROR: Property 'method' in type 'Bar<Bar>' is not assignable to the same property in base type 'Foo<Bar>'.
        return this.optionalProperty;
    }
}


var bar = new Bar();
bar.method(0);
bar.optionalMethod(1);
//~^ ERROR: Cannot invoke an object which is possibly 'undefined'.
bar.property;
bar.optionalProperty;
bar.additionalProperty;
bar.additionalMethod(2);

var obj: {
    method(a: number): string;
    property: string;
    additionalProperty: string;
    additionalMethod(a: number): string;
};

bar = obj;
//~^ ERROR: Variable 'obj' is used before being assigned.
obj = bar;
//~^ ERROR: Type 'Bar' is not assignable to type '{ method: (a: number) => string; property: string; additionalProperty: string; additionalMethod: (a: number) => string; }'.
