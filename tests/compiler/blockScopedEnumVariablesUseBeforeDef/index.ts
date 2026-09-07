// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/blockScopedEnumVariablesUseBeforeDef.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

function foo1() {
    return E.A
    //~^ ERROR: Enum 'E' used before its declaration.
    enum E { A }
}

function foo2() {
    return E.A
    const enum E { A }
}

const config = {
    a: AfterObject.A,
};

const enum AfterObject {
    A = 2,
}
