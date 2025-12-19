// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/quotedModuleNameMustBeAmbient.ts`, Apache-2.0 License

module 'M' {}
//~^ ERROR: Only ambient modules can use quoted names.

declare module 'M2' {}