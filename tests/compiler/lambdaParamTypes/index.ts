// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/lambdaParamTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface MyArrayWrapper<T> {
    constructor(initialItems?: T[]);
    doSomething(predicate: (x: T, y: T) => string): void;
}

declare function create<T>(initialValues?: T[]): MyArrayWrapper<T>;

var thing = create([{ name: "bob", id: 24 }, { name: "doug", id: 32 }]);

// Below should all be OK
thing.doSomething((x, y) => x.name.charAt(0));      // x.name should be string, so should be OK
thing.doSomething((x, y) => x.id.toExponential(0)); // x.id should be string, so should be OK
thing.doSomething((x, y) => y.name.charAt(0));      // x.name should be string, so should be OK
thing.doSomething((x, y) => y.id.toExponential(0)); // x.id should be string, so should be OK

// Below should all be in error
thing.doSomething((x, y) => x.foo); // no such property on x
//~^ ERROR: Property 'foo' does not exist on type '{ name: string; id: number; }'.
thing.doSomething((x, y) => y.foo); // no such property on y
//~^ ERROR: Property 'foo' does not exist on type '{ name: string; id: number; }'.
thing.doSomething((x, y) => x.id.charAt(0));      // x.id should be number, no charAt member
//~^ ERROR: Property 'charAt' does not exist on type 'number'.
thing.doSomething((x, y) => x.name.toExponential(0)); // x.name should be string, no toExponential member
//~^ ERROR: Property 'toExponential' does not exist on type 'string'.
thing.doSomething((x, y) => y.id.charAt(0));
//~^ ERROR: Property 'charAt' does not exist on type 'number'.
thing.doSomething((x, y) => y.name.toExponential(0));
//~^ ERROR: Property 'toExponential' does not exist on type 'string'.
