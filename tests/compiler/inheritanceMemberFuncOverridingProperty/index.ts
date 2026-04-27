// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/inheritanceMemberFuncOverridingProperty.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false


class a {
    x: () => string;
}

class b extends a {
    x() { //~ERROR: Class 'a' defines instance member property 'x', but extended class 'b' defines it as instance member function.
        return "20";
    }
}