// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/indexerAsOptional.ts`, Apache-2.0 License

interface indexSig {
    //Index signatures can't be optional
    [idx?: number]: any; //err
    //~^ ERROR: An index signature parameter cannot have a question mark.
}

class indexSig2 {
    //Index signatures can't be optional
    [idx?: number]: any //err
    //~^ ERROR: An index signature parameter cannot have a question mark.
}