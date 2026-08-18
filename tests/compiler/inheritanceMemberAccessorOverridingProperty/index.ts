// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inheritanceMemberAccessorOverridingProperty.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class a {
    x: string;
}

class b extends a {
    get x() {
      //~^ ERROR: 'x' is defined as a property in class 'a', but is overridden here in 'b' as an accessor.
        return "20";
    }
    set x(aValue: string) {

    }
}
