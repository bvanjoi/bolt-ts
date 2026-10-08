// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/letInLetConstDeclOfForOfAndForIn_ES5.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

for (let let of [1,2,3]) {}
//~^ ERROR: Identifier expected. 'let' is a reserved word in strict mode.
//~| ERROR: 'let' is not allowed to be used as a name in 'let' or 'const' declarations.

for (const let of [1,2,3]) {}
//~^ ERROR: Identifier expected. 'let' is a reserved word in strict mode.
//~| ERROR: 'let' is not allowed to be used as a name in 'let' or 'const' declarations.

for (let let in [1,2,3]) {}
//~^ ERROR: Identifier expected. 'let' is a reserved word in strict mode.
//~| ERROR: 'let' is not allowed to be used as a name in 'let' or 'const' declarations.

for (const let in [1,2,3]) {}
//~^ ERROR: Identifier expected. 'let' is a reserved word in strict mode.
//~| ERROR: 'let' is not allowed to be used as a name in 'let' or 'const' declarations.

{
	for (let let of [1,2,3]) {}
//~^ ERROR: Identifier expected. 'let' is a reserved word in strict mode.
//~| ERROR: 'let' is not allowed to be used as a name in 'let' or 'const' declarations.

	for (const let of [1,2,3]) {}
//~^ ERROR: Identifier expected. 'let' is a reserved word in strict mode.
//~| ERROR: 'let' is not allowed to be used as a name in 'let' or 'const' declarations.
	
	for (let let in [1,2,3]) {}
//~^ ERROR: Identifier expected. 'let' is a reserved word in strict mode.
//~| ERROR: 'let' is not allowed to be used as a name in 'let' or 'const' declarations.

	for (const let in [1,2,3]) {}
//~^ ERROR: Identifier expected. 'let' is a reserved word in strict mode.
//~| ERROR: 'let' is not allowed to be used as a name in 'let' or 'const' declarations.
}


