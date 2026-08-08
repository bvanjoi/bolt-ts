// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parametersSyntaxErrorNoCrash3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

// https://github.com/microsoft/TypeScript/issues/59449

export function getHtml({
    //~^ ERROR: Function implementation is missing or not immediately following the declaration.
    //~| ERROR: Function implementation is missing or not immediately following the declaration.
    //~| ERROR: 'getHtml', which lacks return-type annotation, implicitly has an 'any' return type.
    return  " string" // a long string;
    //~^ ERROR: Expected ':'.
    //~| ERROR: Identifier expected.
    //~| ERROR: Expected ','.
    //~| ERROR: '' is an unused renaming of 'return'. Did you intend to use it as a type annotation?
}
//~^ ERROR: Expected ':'.
//~| ERROR: Identifier expected.
//~| ERROR: '' is an unused renaming of ' string'. Did you intend to use it as a type annotation?
//~ ERROR: Expected ')'.