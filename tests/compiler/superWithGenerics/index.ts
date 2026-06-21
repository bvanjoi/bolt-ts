// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/superWithGenerics.ts`, Apache-2.0 License

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
