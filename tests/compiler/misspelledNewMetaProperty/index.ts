// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/misspelledNewMetaProperty.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo(){new.targ}
//~^ ERROR: 'targ' is not a valid meta-property for keyword 'new'. Did you mean 'target'?