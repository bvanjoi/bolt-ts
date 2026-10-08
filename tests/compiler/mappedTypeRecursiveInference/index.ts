// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mappedTypeRecursiveInference.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es6,dom]

interface A { a: A }
declare let a: A;
type Deep<T> = { [K in keyof T]: Deep<T[K]> }
declare function foo<T>(deep: Deep<T>): T;
const out = foo(a);
out.a
out.a.a
out.a.a.a.a.a.a.a


interface B { [s: string]: B }
declare let b: B;
const oub = foo(b);
oub.b
oub.b.b
oub.b.a.n.a.n.a

declare let xhr: XMLHttpRequest;
const out2 = foo(xhr);
//~^ ERROR: Argument of type 'XMLHttpRequest' is not assignable to parameter of type 'Deep<Deep<T>>'.
out2.responseXML
out2.responseXML.activeElement.className.length
