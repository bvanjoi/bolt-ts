// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/letInVarDeclOfForIn_ES5.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

// should not be an error
for (var let in [1,2,3]) {}
//~^ ERROR: Identifier expected. 'let' is a reserved word in strict mode.

{
	for (var let in [1,2,3]) {}
//~^ ERROR: Identifier expected. 'let' is a reserved word in strict mode.
}