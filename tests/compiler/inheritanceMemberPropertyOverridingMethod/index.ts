// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/inheritanceMemberPropertyOverridingMethod.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class a {
    x() {
        return "20";
    }
}

class b extends a {
    x: () => string;
}