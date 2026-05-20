// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportEqualsClassNoRedeclarationError.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

class SomeClass {
    static get someProp(): number {
        return 0;
    }

    static set someProp(value: number) {}
}
export = SomeClass;