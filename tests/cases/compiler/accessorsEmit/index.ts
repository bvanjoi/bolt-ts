// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/accessorsEmit.ts`, Apache-2.0 License

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