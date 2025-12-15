// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/illegalModifiersOnClassElements.ts`, Apache-2.0 License

class C {
    declare foo = 1;
    //~^ ERROR: Initializers are not allowed in ambient contexts.
    export bar = 1;
    //~^ ERROR: 'export' modifier cannot appear on class elements of this kind.
    export emm() {}
    //~^ ERROR: 'export' modifier cannot appear on class elements of this kind.
    export get taz() { return 1 }
    //~^ ERROR: 'export' modifier cannot appear on class elements of this kind.
    export set taz() {}
    //~^ ERROR: 'export' modifier cannot appear on class elements of this kind.
    //~| ERROR: A 'set' accessor must have exactly one parameter.
    get baz() { return 1 }
    set baz() {}
    //~^ ERROR: A 'set' accessor must have exactly one parameter.

    get tcc() { return 1}
    set tcc(a: number, b: number, c: number) { }
    //~^ ERROR: A 'set' accessor must have exactly one parameter.

    get taa(v: number) { return 1 }
    //~^ ERROR: A 'get' accessor cannot have parameters.
    set taa(v: number) { }
}
