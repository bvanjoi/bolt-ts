// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parametersSyntaxErrorNoCrash2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

// https://github.com/microsoft/TypeScript/issues/59353

export default function getThing( { return 'thing'; }
//~^ ERROR: Expected ':'.
//~| ERROR: Identifier expected.
//~| ERROR: Identifier expected.
//~| ERROR: Expected ','.
//~| ERROR: Expected ':'.
//~| ERROR: Expected ','.
//~| ERROR: '' is an unused renaming of 'return'. Did you intend to use it as a type annotation?
//~| ERROR: '' is an unused renaming of 'thing'. Did you intend to use it as a type annotation?
//~| ERROR: Function implementation is missing or not immediately following the declaration.
//~| ERROR: Function implementation is missing or not immediately following the declaration.
//~| ERROR: 'getThing', which lacks return-type annotation, implicitly has an 'any' return type.
//~ ERROR: Expected ')'.