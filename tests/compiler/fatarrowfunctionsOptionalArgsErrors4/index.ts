// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/fatarrowfunctionsOptionalArgsErrors4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

    false ? (arg?: number = 0) => 47 : null;        //~ ERROR: Parameter cannot have question mark and initializer.
    false ? ((arg?: number = 0) => 57) : null;      //~ ERROR: Parameter cannot have question mark and initializer.
    false ? null : (arg?: number = 0) => 67;        //~ ERROR: Parameter cannot have question mark and initializer.
    ((arg?:number = 1) => 0) + '' + ((arg?:number = 2) => 106);
    //~^ ERROR: Parameter cannot have question mark and initializer.
    //~| ERROR: Parameter cannot have question mark and initializer.

    foo(  //~ERROR: Cannot find name 'foo'.
        (a) => 110, 
        ((a) => 111), 
        (a) => {
            return 112;
        },
        (a? ) => 113, 
        (a, b? ) => 114, 
        (a: number) => 115, 
        (a: number = 0) => 116, 
        (a = 0) => 117, 
        (a?: number = 0) => 118, 
    //~^ ERROR: Parameter cannot have question mark and initializer.
        (...a: number[]) => 119, 
        (a, b? = 0, ...c: number[]) => 120,
    //~^ ERROR: Parameter cannot have question mark and initializer.
        (a) => (b) => (c) => 121,
        false? (a) => 0 : (b) => 122
    );