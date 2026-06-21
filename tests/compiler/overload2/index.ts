// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overload2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

enum A { }
enum B { }
    
function foo(a: A);
function foo(b: B);
// should be ok
function foo(x: number) { 
}

class C { }
function foo1(a: A);
function foo1(c: C);
// should be ok
function foo1(x: number) { 
}
