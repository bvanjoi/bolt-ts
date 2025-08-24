// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/primitiveTypeAsInterfaceNameGeneric.ts`, Apache-2.0 License

interface number<T> {}
//~^ ERROR: Interface name cannot be 'number'.