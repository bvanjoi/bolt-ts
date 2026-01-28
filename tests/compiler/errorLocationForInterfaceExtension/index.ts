// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/errorLocationForInterfaceExtension.ts`, Apache-2.0 License

var n = '';

interface x extends string { }
//~^ ERROR: An interface cannot extend a primitive type like 'string'.
//~| ERROR: An interface can only extend an object type or intersection of object types with statically known members.
