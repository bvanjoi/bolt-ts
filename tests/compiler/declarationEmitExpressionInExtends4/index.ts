// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitExpressionInExtends4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

function getSomething() {
    return class D { }
}

class C extends getSomething()<number, string> {
  //~^ ERROR: Type 'D' is not generic.

}

class C2 extends SomeUndefinedFunction()<number, string> {
  //~^ ERROR: Cannot find name 'SomeUndefinedFunction'.
}


class C3 extends SomeUndefinedFunction {
  //~^ ERROR: Cannot find name 'SomeUndefinedFunction'.
}