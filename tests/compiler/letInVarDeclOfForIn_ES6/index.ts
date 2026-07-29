// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/letInVarDeclOfForIn_ES6.ts`, Apache-2.0 License

//@compiler-options: target=es6

// should not be an error
for (var let in [1,2,3]) {}
//~^ ERROR: Identifier expected. 'let' is a reserved word in strict mode.

{
	for (var let in [1,2,3]) {}
//~^ ERROR: Identifier expected. 'let' is a reserved word in strict mode.
}