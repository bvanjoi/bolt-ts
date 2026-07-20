// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericInheritedDefaultConstructors.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Constructor<T> {
    new(...args: any[]): T;
    prototype: T;
}

class A<U> { a: U; }
//~^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.
class B<V> extends A<V> { b: V; }
//~^ ERROR: Property 'b' has no initializer and is not definitely assigned in the constructor.
var c:Constructor<B<boolean>> = B; // shouldn't error here
