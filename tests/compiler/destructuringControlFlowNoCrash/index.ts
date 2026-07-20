// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructuringControlFlowNoCrash.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

// legal JS, if nonsensical, which also triggers the issue
const {
  date,
  //~^ ERROR: Property '"date"' does not exist on type '(inspectedElement: any) => number'.
} = (inspectedElement: any) => 0;

date.toISOString();

// Working flow code
const {
  date2,
  //~^ ERROR: Property '"date2"' does not exist on type '(inspectedElement: any) => error'.
} = (inspectedElement: any).props;
//~^ ERROR: Expected '=>'.
//~| ERROR: Identifier expected.
//~| ERROR: Expected ','.
//~| ERROR: Declarations must be initialized.

date2.toISOString();

// It could also be an async function
const { constructor } = async () => {};

