// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/letInConstDeclarations_ES6.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// All use of let in const declaration should be an error
const x = 50, let = 5;
//~^ ERROR: Identifier expected. 'let' is a reserved word in strict mode.
//~| ERROR: 'let' is not allowed to be used as a name in 'let' or 'const' declarations.

{
    const x = 10, let = 20;
    //~^ ERROR: Identifier expected. 'let' is a reserved word in strict mode.
    //~| ERROR: 'let' is not allowed to be used as a name in 'let' or 'const' declarations.
}