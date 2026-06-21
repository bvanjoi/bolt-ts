// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inheritanceMemberPropertyOverridingAccessor.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class a {
    private __x: () => string;
    get x() {
        return this.__x;
    }
    set x(aValue: () => string) {
        this.__x = aValue;
    }
}

class b extends a {
    x: () => string;
    //~^ ERROR: 'x' is defined as an accessor in class 'a', but is overridden here in 'b' as an instance property.
}