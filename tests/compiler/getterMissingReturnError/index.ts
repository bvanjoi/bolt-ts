// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/getterMissingReturnError.ts`, Apache-2.0 License

class test {
    public get p2(){    //~ ERROR: A 'get' accessor must return a value. 

    }
}
