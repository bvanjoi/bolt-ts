// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/staticIndexSignatureAndNormalIndexSignature.ts`, Apache-2.0 License

class Foo {
  [p: string]: any;
  static [p: string]: number;
}

let a: string = Foo['']
//~^ ERROR: Type 'number' is not assignable to type 'string'.


class Bar {
  static [p: string]: number;
}

let b: string = Bar['']
//~^ ERROR: Type 'number' is not assignable to type 'string'.