// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/inheritanceStaticFuncOverridingAccessor.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class a {
    static get x() {
        return "20";
    }
    static set x(aValue: string) {

    }
}

class b extends a { //~ERROR: Class static side 'typeof b' incorrectly extends base class static side 'typeof a'.
    static x() {
        return "20";
    }
}