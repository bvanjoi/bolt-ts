// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/weakTypeAndPrimitiveNarrowing.ts`, Apache-2.0 License

//@compiler-options: target=esnext

type LiteralsAndWeakTypes = 
  | 'A'
  | 'B'
  | { optional?: true }
  | { toLowerCase?(): string }
  | { toUpperCase?(): string, otherOptionalProp?: number };

const g = (arg: LiteralsAndWeakTypes) => {
    if (arg === 'A') {
      arg;
    } else {
      arg;
    }
}

type PrimitivesAndWeakTypes =
  | string
  | number
  | { optional?: true }
  | { toLowerCase?(): string }
  | { toUpperCase?(): string, otherOptionalProp?: number };

const h = (arg: PrimitivesAndWeakTypes) => {
    if (arg === 'A') {
      arg;
    } else {
      arg;
    }
}
