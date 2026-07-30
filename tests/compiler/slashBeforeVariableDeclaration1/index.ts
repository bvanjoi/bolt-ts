// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/slashBeforeVariableDeclaration1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

\ declare var v;
//~^ ERROR: Invalid character.
//~| ERROR: Declaration or statement expected.
//~| ERROR: Variable 'v' implicitly has an 'any' type.
