// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadOnConstConstraintChecks2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A {} 
class B extends A {}
class C extends A {
    public foo() { }
}
function foo(name: 'hi'): B;
function foo(name: 'bye'): C;
function foo(name: string): A;
function foo(name: any): A {
    return null;
    //~^ ERROR: Type 'null' is not assignable to type 'A'. 
}
