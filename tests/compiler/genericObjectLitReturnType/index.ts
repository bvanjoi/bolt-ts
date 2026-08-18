// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericObjectLitReturnType.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class X<T>
{
    f(t: T) { return { a: t }; }
}

 
var x: X<number>;
var t1 = x.f(5);
//~^ ERROR: Variable 'x' is used before being assigned.
//~| ERROR: Variable 'x' is used before being assigned.
t1.a = 5; // Should not error: t1 should have type {a: number}, instead has type {a: T}
 
