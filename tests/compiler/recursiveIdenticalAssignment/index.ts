// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveIdenticalAssignment.ts`, Apache-2.0 License

//@compiler-options: target=es2015
interface A<T> {
    x: A<T>
}

interface B<T extends B<B<T>>> { // error, constraint referencing itself
    x: B<T>
}

var a: A<A<any>>
var b: B<B<any>> = a // Error, any does not satisfy constraint B<B<T>>
//~^ ERROR: Variable 'a' is used before being assigned.
