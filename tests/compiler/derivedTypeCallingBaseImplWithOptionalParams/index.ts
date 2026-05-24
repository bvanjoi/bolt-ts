// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/derivedTypeCallingBaseImplWithOptionalParams.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface MyInterface {
    myMethod(...myList: any[]);
}
class MyClass implements MyInterface {
    myMethod(myList: any[]) { // valid
    }
}

var x: MyInterface = new MyClass();
x.myMethod(); // should be valid, but MyClass has no implementation to handle it.

var y: MyClass = new MyClass();
y.myMethod(); // error
//~^ ERROR: Expected 1 arguments, but got 0.