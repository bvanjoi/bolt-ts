// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/promiseTest.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Promise<T> {
    then<A>(success?: (value: T) => Promise<A>): Promise<A>;
    then<B>(success?: (value: T) => B): Promise<B>;
    data: T;
}

var p: Promise<number> = null;
//~^ ERROR: Type 'null' is not assignable to type 'Promise<number>'.
var p2 = p.then(function (x) {
    return p;
} );

var x = p2.data; // number

