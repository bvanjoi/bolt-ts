// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/inheritanceStaticMembersIncompatible.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: allowUnreachableCode

class a {
    static get x(): () => string {
        return null;;
    }
    static set x(aValue: () => string) {
    }
}

class b extends a {
    static x: () => string;
}