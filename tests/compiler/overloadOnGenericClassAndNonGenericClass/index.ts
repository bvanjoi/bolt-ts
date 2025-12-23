// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/overloadOnGenericClassAndNonGenericClass.ts`, Apache-2.0 License

class A { a; }
class B { b; }
class C { c; }
class X<T> { x: T; }
class X1 { x: string; }
class X2 { x: string; }
function f(a: X1): A;
function f<T>(a: X<T>): B;
function f(a): any {
}

var xs: X<string>;

var t3 = f(xs);
var t3: A; // should not error
