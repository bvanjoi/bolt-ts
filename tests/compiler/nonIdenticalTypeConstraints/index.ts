// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nonIdenticalTypeConstraints.ts`, Apache-2.0 License

//@compiler-options: target=es2015
class Different {
    a: number;
    //~^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor. 
    b: string;
    //~^ ERROR: Property 'b' has no initializer and is not definitely assigned in the constructor. 
    c: boolean;
    //~^ ERROR: Property 'c' has no initializer and is not definitely assigned in the constructor. 
}

class Foo<T extends Function> {
    //~^ ERROR: All declarations of 'Foo' must have identical type parameters.
    n: T;
    //~^ ERROR: Property 'n' has no initializer and is not definitely assigned in the constructor.
}
interface Foo<T extends Different> {
    //~^ ERROR: All declarations of 'Foo' must have identical type parameters.
    y: T;
}
interface Qux<T extends Different> {
    //~^ ERROR: All declarations of 'Qux' must have identical type parameters.
    y: T;
}
class Qux<T extends Function> {
    //~^ ERROR: All declarations of 'Qux' must have identical type parameters.
    n: T;
    //~^ ERROR: Property 'n' has no initializer and is not definitely assigned in the constructor.
}

class Bar<T extends Function> {
    n: T;
    //~^ ERROR: Property 'n' has no initializer and is not definitely assigned in the constructor.
}
interface Bar<T extends Function> {
    y: T;
}
interface Baz<T extends Function> {
    y: T;
}
class Baz<T extends Function> {
    n: T;
    //~^ ERROR: Property 'n' has no initializer and is not definitely assigned in the constructor.
}

class Quux<T> {
    //~^ ERROR: All declarations of 'Quux' must have identical type parameters.
    n: T;
    //~^ ERROR: Property 'n' has no initializer and is not definitely assigned in the constructor.
}
interface Quux<U> {
    //~^ ERROR: All declarations of 'Quux' must have identical type parameters.
    m: U;
}