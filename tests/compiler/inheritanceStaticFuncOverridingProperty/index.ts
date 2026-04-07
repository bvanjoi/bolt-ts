// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/inheritanceStaticFuncOverridingProperty.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class a {
    static x: string;
}

class b extends a {
  //~^ ERROR: Class static side 'typeof b' incorrectly extends base class static side 'typeof a'.
    static x() {
        return "20";
    }
}