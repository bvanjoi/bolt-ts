// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/infinitelyExpandingTypes2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Foo<T> {
    x: Foo<Foo<T>>;
} 

interface Bar<T> extends Foo<T> {
    y: string;
}

function f(p: Foo<number>) {
    console.log(p);
}

var v: Bar<number> = null;
//~^ ERROR: Type 'null' is not assignable to type 'Bar<number>'.

f(v); // should not error
