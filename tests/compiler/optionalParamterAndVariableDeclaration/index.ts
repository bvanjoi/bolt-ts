// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/optionalParamterAndVariableDeclaration.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks=false

class C {
    constructor(options?: number) {
        var options = (options || 0);
    }
}
