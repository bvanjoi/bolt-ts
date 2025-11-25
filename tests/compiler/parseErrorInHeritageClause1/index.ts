// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/parseErrorInHeritageClause1.ts`, Apache-2.0 License

class C extends A ¬ {
//~^ ERROR: Cannot find name 'A'.
//~| ERROR: Invalid character.
}