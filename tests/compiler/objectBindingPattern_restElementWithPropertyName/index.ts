// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/objectBindingPattern_restElementWithPropertyName.ts`, Apache-2.0 License

const { ...a: b } = {};
//~^ ERROR: A rest element cannot have a property name.