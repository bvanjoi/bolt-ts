// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/accessorWithRestParam.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

class C {
    set X(...v) { }
    //~^ ERROR: A 'set' accessor cannot have rest parameter
    static set X(...v2) { }
    //~^ ERROR: A 'set' accessor cannot have rest parameter
}