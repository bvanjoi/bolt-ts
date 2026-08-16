// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mutuallyRecursiveGenericBaseTypes1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface A<T> {
    foo(): B<T>; // instead of B does see this
    foo(): void; // instead of B does see this
 
    foo2(): B<number>;
}
 
interface B<T> extends A<T> {
    bar(): void;
}
 
var b: B<number>;
b.foo(); // should not error
//~^ ERROR: Variable 'b' is used before being assigned.
 
