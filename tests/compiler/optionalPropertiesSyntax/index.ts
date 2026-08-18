// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/optionalPropertiesSyntax.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface fnSigs {
    //functions signatures can be optional
    fn(): void;
    fn?(): void; //err
    //~^ ERROR: Overload signatures must all be optional or required.
    fn2?(): void;
}

interface callSig {
    //Call signatures can't be optional
    (): any;
    ()?: any; //err
    //~^ ERROR: Property or signature expected.
    //~| ERROR: Property or signature expected.
    ?(): any; //err
    //~^ ERROR: Property or signature expected.
}

interface constructSig {
    //Construct signatures can't be optional
    new (): any;
    new ()?: any; //err
    //~^ ERROR: Property or signature expected.
    //~| ERROR: Property or signature expected.
    new ?(): any; //err
}

interface propertySig {
    //Property signatures can be optional
    prop: any;
    //~^ ERROR: All declarations of 'prop' must have identical modifiers.
    prop?: any;
    //~^ ERROR: Duplicate identifier 'prop'.
    prop2?: any;
}

interface indexSig {
    //Index signatures can't be optional
    [idx: number]: any;
    //~^ ERROR: Duplicate index signature for type 'number'.
    [idx: number]?: any; //err
    //~^ ERROR: Duplicate index signature for type 'number'.
    //~| ERROR: An index signature must have a type annotation.
    //~| ERROR: Property or signature expected.
    //~| ERROR: Property or signature expected.
    ? [idx: number]: any; //err
    //~^ ERROR: Property or signature expected.
    //~| ERROR: Duplicate index signature for type 'number'.
    [idx?: number]: any; //err
    //~^ ERROR: An index signature parameter cannot have a question mark.
    //~| ERROR: Duplicate index signature for type 'number'.
}