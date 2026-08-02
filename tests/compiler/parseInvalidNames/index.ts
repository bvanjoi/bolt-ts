// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parseInvalidNames.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace 100 {}
//~^ ERROR: Cannot find name 'namespace'.
//~| ERROR: Unexpected keyword or identifier.
//~| ERROR: Unexpected keyword or identifier.
interface 100 {}
//~^ ERROR: Cannot find name 'interface'.
//~| ERROR: Unexpected keyword or identifier.
//~| ERROR: Unexpected keyword or identifier.
type 100 {}
//~^ ERROR: Cannot find name 'type'.
//~| ERROR: Unexpected keyword or identifier.
//~| ERROR: Unexpected keyword or identifier.

export namespace 100 {}
//~^ ERROR: Declaration or statement expected.
//~| ERROR: Cannot find name 'namespace'.
//~| ERROR: Unexpected keyword or identifier.
//~| ERROR: Unexpected keyword or identifier.
export interface 100 {}
//~^ ERROR: Declaration or statement expected.
//~| ERROR: Cannot find name 'interface'.
//~| ERROR: Unexpected keyword or identifier.
//~| ERROR: Unexpected keyword or identifier.
export type 100 {}
//~^ ERROR: Declaration or statement expected.
//~| ERROR: Cannot find name 'type'.
//~| ERROR: Unexpected keyword or identifier.
//~| ERROR: Unexpected keyword or identifier.

