// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/primitiveTypeAsInterfaceNameGeneric.ts`, Apache-2.0 License

interface number<T> {}
//~^ ERROR: Interface name cannot be 'number'.