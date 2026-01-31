// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/inheritanceStaticAccessorOverridingMethod.ts`, Apache-2.0 License

class a {
    static x() {
        return "20";
    }
}

class b extends a {
//~^ ERROR: Class static side 'typeof b' incorrectly extends base class static side 'typeof a'.
    static get x() {
        return "20";
    }
    static set x(aValue: string) {

    }
}

