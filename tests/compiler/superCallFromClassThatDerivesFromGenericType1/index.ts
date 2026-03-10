// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/superCallFromClassThatDerivesFromGenericType1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

declare class B<T> {
    m<U>(): B<U>;
    static g(): B<any>;
}

class D extends B<any> {
    constructor() {
        super();
    }
}