// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionRestParameterClassMethod.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class c1 {
    public foo(_i: number, ...restParameters) { //_i is error
        var _i = 10; // no error
    }
    public fooNoError(_i: number) { // no error
        var _i = 10; // no error
    }
    public f4(_i: number, ...rest); // no codegen no error
    public f4(_i: string, ...rest); // no codegen no error
    public f4(_i: any, ...rest) { // error
        var _i: any; // no error
    }

    public f4NoError(_i: number); // no error
    public f4NoError(_i: string); // no error
    public f4NoError(_i: any) { // no error
        var _i: any; // no error
    }
}

declare class c2 {
    public foo(_i: number, ...restParameters); // No error - no code gen
    public fooNoError(_i: number); // no error

    public f4(_i: number, ...rest); // no codegen no error
    public f4(_i: string, ...rest); // no codegen no error
    public f4NoError(_i: number); // no error
    public f4NoError(_i: string); // no error
}

class c3 {
    public foo(...restParameters) {
        var _i = 10; // no error
    }
    public fooNoError() {
        var _i = 10; // no error
    }
}