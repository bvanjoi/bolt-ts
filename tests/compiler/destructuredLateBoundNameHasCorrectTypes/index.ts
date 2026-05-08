// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructuredLateBoundNameHasCorrectTypes.ts`, Apache-2.0 License

//@compiler-options: target=es6

let { [Symbol.iterator]: destructured } = [];
void destructured;

const named = "prop";

let { [named]: computed } = { prop: "b" };
void computed;

const notPresent = "prop2";

let { [notPresent]: computed2 } = { prop: "b" };
//~^ ERROR: Property '"prop2"' does not exist on type '{ prop: string; }'.
//~| ERROR: Object literal may only specify known properties, and 'prop' does not exist in type '{ prop2: any; }'.
