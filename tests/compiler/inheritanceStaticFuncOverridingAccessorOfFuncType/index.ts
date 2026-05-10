// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/inheritanceStaticFuncOverridingAccessorOfFuncType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class a {
    static get x(): () => string {
        return null;
    }
}

class b extends a {
    static x() {
        return "20";
    }
}