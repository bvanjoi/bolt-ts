// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inheritanceMemberFuncOverridingAccessor.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class a {
    get x() {
        return "20";
    }
    set x(aValue: string) {

    }
}

class b extends a {
    x() {
      //~^ ERROR: Class 'a' defines instance member accessor 'x', but extended class 'b' defines it as instance member function.
      //~| ERROR: Property 'x' in type 'b<b>' is not assignable to the same property in base type 'a<b>'.
        return "20";
    }
}