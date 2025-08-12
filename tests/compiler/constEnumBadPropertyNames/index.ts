// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/constEnumBadPropertyNames.ts`, Apache-2.0 License

const enum E { A }
var x = E["B"] 
//~^ ERROR: Property '"B"' does not exist on type 'typeof E'.