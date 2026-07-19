// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/accessorsEmit.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class Result { }

class Test {
    get Property(): Result {
        var x = 1;
        return null;
    }
}

class Test2 {
    get Property() {
        var x = 1;
        return null;
    }
}
