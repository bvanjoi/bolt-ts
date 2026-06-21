// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferSetterParamType.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Foo {

    get bar() {
        return 0;
    }
    set bar(n) { // should not be an error - infer number
    }
}

class Foo2 {

    get bar() {
        return 0; // should be an error - can't coerce infered return type to match setter annotated type
        //~^ ERROR: Type 'number' is not assignable to type 'string'.
    }
    set bar(n:string) {
    }
}
