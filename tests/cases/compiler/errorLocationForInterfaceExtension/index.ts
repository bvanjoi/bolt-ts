// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/errorLocationForInterfaceExtension.ts`, Apache-2.0 License

var n = '';

interface x extends string { }
//~^ ERROR: An interface cannot extend a primitive type like 'string'.
