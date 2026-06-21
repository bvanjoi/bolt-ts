// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/promisesWithConstraints.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Promise<T> {
    then<U>(cb: (x: T) => Promise<U>): Promise<U>;
}

interface CPromise<T extends { x: any; }> {
    then<U extends { x: any; }>(cb: (x: T) => Promise<U>): Promise<U>;
}

interface Foo { x: any; }
interface Bar { x: any; y: any; }

var a: Promise<Foo>;
declare var b: Promise<Bar>;
a = b; // ok
b = a; // ok
//~^ ERROR: Property 'y' is missing.

var a2: CPromise<Foo>;
declare var b2: CPromise<Bar>;
a2 = b2; // ok
b2 = a2; // was error
//~^ ERROR: Property 'y' is missing.
