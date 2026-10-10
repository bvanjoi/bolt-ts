// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfaceNaming1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
interface { }
//~^ ERROR: Unexpected keyword or identifier.
//~| ERROR: Cannot find name 'interface'.
interface interface{ }
//~^ ERROR: Identifier expected. 'interface' is a reserved word in strict mode.
interface & { }
//~^ ERROR: Cannot find name 'interface'.
