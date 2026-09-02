// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inheritanceMemberAccessorOverridingMethod.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

class a {
    x() {
        return "20";
    }
}

class b extends a {
    get x() {
      //~^ ERROR: Class 'a' defines instance member function 'x', but extended class 'b' defines it as instance member accessor.
        return () => "20";
    }
    set x(aValue) {
        
    }
}
