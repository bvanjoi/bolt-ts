// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/constructorParametersInVariableDeclarations.ts`, Apache-2.0 License

class A {
    private a = x;
    //~^ ERROR: Cannot find name 'x'.
    private b = { p: x };
    //~^ ERROR: Cannot find name 'x'.
    private c = () => x;
    //~^ ERROR: Cannot find name 'x'.
    constructor(x: number) {
    }
}

class B {
    private a = x;
    //~^ ERROR: Cannot find name 'x'.
    private b = { p: x };
    //~^ ERROR: Cannot find name 'x'.
    private c = () => x;
    //~^ ERROR: Cannot find name 'x'.
    constructor() {
        var x = 1;
    }
}