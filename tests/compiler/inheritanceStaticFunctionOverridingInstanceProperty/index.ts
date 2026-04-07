// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/inheritanceStaticFunctionOverridingInstanceProperty.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class a {
    x: string;
}

class b extends a {
    static x() {
        return new b().x;
    }
}