// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeofUnknownSymbol.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// previously gave no error here
var x = typeof whatsthis
//~^ ERROR: Cannot find name 'whatsthis'.
