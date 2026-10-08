// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/accessorParameterAccessibilityModifier.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

class C {
    set X(public v) { }
    //~^ ERROR: A parameter property is only allowed in a constructor implementation.
    static set X(public v2) { }
    //~^ ERROR: A parameter property is only allowed in a constructor implementation.
}