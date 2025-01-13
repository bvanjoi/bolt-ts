// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/stringIndexerAndConstructor.ts`, Apache-2.0 License

class C {
  [s: string]: number;
  constructor() { }
  static v() { }
}

interface I {
  [s: string]: number;
  (): boolean;
  new (): boolean;
  "": string;
  //~^ ERROR: Property '""' of type 'string' is not assignable to 'string' index type 'number'
  d: string;
  //~^ ERROR: Property 'd' of type 'string' is not assignable to 'string' index type 'number'
}