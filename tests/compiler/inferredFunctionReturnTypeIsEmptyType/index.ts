// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferredFunctionReturnTypeIsEmptyType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: allowUnreachableCode

function foo() {
    if (true) {
        return 42;
    }
    else {
        return "42";
    }
};
