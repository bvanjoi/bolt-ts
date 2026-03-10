// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/superCallFromClassThatDerivesFromGenericType2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

declare class B<T> {
    m<U>(): B<U>;
}

class D extends B<any> {
    constructor() {
        super();
    }
}