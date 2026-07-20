// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parseInvalidNames.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace 100 {}
//~^ ERROR: Cannot find name 'namespace'.
interface 100 {}
//~^ ERROR: Cannot find name 'interface'.
type 100 {}
//~^ ERROR: Cannot find name 'type'.

export namespace 100 {}
//~^ ERROR: Declaration or statement expected.
//~| ERROR: Cannot find name 'namespace'.
export interface 100 {}
//~^ ERROR: Declaration or statement expected.
//~| ERROR: Cannot find name 'interface'.
export type 100 {}
//~^ ERROR: Declaration or statement expected.
//~| ERROR: Cannot find name 'type'.

