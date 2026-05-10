// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/objectCreationExpressionInFunctionParameter.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks=false
//@run-fail

class A {
    constructor(public a1: string) {
    }
}
function foo(x = new A(123)) { //should error, 123 is not string
  //~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
}}//~ERROR: Declaration or statement expected.