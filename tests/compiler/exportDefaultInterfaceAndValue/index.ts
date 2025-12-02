// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/exportDefaultInterfaceAndValue.ts`, Apache-2.0 License

export default interface A { a: string; }
export default function() { return 1; }
declare var x: A;
