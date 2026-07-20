// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/crashInsourcePropertyIsRelatableToTargetProperty.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C {
    private x = 1;
}
class D extends C { }
function foo(x: "hi", items: string[]): typeof foo;
function foo(x: string, items: string[]): typeof foo {
    return null;
    //~^ ERROR: Type 'null' is not assignable to type '(x: "hi", items: string[]) => any'.
}
var a: D = foo("hi", []);
//~^ ERROR: Type '(x: "hi", items: string[]) => any' is not assignable to type 'D'.
