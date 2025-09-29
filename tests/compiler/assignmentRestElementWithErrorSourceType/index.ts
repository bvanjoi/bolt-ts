// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/assignmentRestElementWithErrorSourceType.ts`, Apache-2.0 License

var tuple: [string, number];
[...c] = tupel; // intentionally misspelled
//~^ ERROR: Cannot find name 'c'.
//~| ERROR: Cannot find name 'tupel'.