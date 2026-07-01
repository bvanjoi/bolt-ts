// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericAssignmentCompatWithInterfaces1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Comparable<T> {
   compareTo(other: T): number;
}
interface I<T> {
    x: Comparable<T>;
}
interface K<T> {
   x: A<T>;
}
class A<T> implements Comparable<T> { compareTo(other: T) { return 1; } }
var z = { x: new A<number>() };
var a1: I<string> = { x: new A<number>() };
//~^ ERROR: Type 'A<number>' is not assignable to type 'Comparable<string>'.
var a2: I<string> = function (): { x: A<number> } {
//~^ ERROR: Type '{ x: A<number>; }' is not assignable to type 'I<string>'.
   var z = { x: new A<number>() }; return z;
} ();
var a3: I<string> = z;
//~^ ERROR: Type '{ x: A<number>; }' is not assignable to type 'I<string>'.
var a4: I<string> = <K<number>>z;
//~^ ERROR: Type 'K<number>' is not assignable to type 'I<string>'. 
