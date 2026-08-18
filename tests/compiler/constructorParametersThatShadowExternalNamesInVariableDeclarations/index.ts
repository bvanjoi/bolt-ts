// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constructorParametersThatShadowExternalNamesInVariableDeclarations.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var x = 1;
class A {
    private a = x;
    //~^ ERROR: Initializer of instance member variable 'a' cannot reference identifier 'x' declared in the constructor.
    constructor(x: number) {
    }
}

class B {
    private a = x;
    //~^ ERROR: Initializer of instance member variable 'a' cannot reference identifier 'x' declared in the constructor.
    constructor() {
        var x = "";
    }
}