// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/concatClassAndString.ts`, Apache-2.0 License

class f { }

f += ''; //~ ERROR: Cannot assign to 'f' because it is a class.
