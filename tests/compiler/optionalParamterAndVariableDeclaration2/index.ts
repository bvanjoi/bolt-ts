// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/optionalParamterAndVariableDeclaration2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks

class C {
    constructor(options?: number) {
        var options = (options || 0);
        //~^ ERROR: Subsequent variable declarations must have the same type. Variable 'options' must be of type 'undefined | number', but here has type 'number'.
    }
}