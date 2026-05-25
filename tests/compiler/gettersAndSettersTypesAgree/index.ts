// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/gettersAndSettersTypesAgree.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C {
    public get Foo() { return "foo";} // ok
    public set Foo(foo) {} // ok - type inferred from getter return statement

    public get Bar() { return "foo";} // ok
    public set Bar(bar:string) {} // ok - type must be declared
}

var o1 = {get Foo(){return 0;}, set Foo(val){}}; // ok - types agree (inference)
var o2 = {get Foo(){return 0;}, set Foo(val:number){}}; // ok - types agree