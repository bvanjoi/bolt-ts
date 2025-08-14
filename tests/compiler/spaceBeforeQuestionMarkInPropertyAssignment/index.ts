// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/spaceBeforeQuestionMarkInPropertyAssignment.ts`, Apache-2.0 License

var x = {x ?: 1} // should not crash
//~^ ERROR: An object member cannot be declared optional.