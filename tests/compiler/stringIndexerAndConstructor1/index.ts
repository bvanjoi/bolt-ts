// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/stringIndexerAndConstructor1.ts`, Apache-2.0 License

interface I {
  [s: string]: number;
  "": string;
  //~^ ERROR: Property '""' of type 'string' is not assignable to 'string' index type 'number'
}