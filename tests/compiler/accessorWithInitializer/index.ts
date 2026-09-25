// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/accessorWithInitializer.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

class C {
    set X(v = 0) { }
    //~^ ERROR: A 'set' accessor parameter cannot have an initializer.
    static set X(v2 = 0) { }
    //~^ ERROR: A 'set' accessor parameter cannot have an initializer.
}
