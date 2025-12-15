// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/exportDefaultInterfaceAndTwoFunctions.ts`, Apache-2.0 License

export default interface A { a: string; }
//~^ ERROR: Cannot redeclare exported variable 'default'.
export default function() { return 1; }
//~^ ERROR: Cannot redeclare exported variable 'default'.
//~| ERROR: Duplicate function implementation.
export default function() { return 2; }
//~^ ERROR: Cannot redeclare exported variable 'default'.
//~| ERROR: Duplicate function implementation.
