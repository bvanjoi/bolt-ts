// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/missingReturnStatement1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Foo {
    foo(): number { //~ERROR: A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value.
        //return 4;
    }
}
