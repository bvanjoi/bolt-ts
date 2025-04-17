// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/mappedTypeWithCombinedTypeMappers.ts`, Apache-2.0 License

type Meta<T, A> = {
  readonly[P in keyof T]: {
      value: T[P];
      also: A;
      readonly children: Meta<T[P], A>;
  };
}

interface Input {
  x: string;
  y: number;
}

declare const output: Meta<Input, boolean>;

const shouldFail: { important: boolean } = output.x.children;
//~^ ERROR: Type 'string' is not assignable to type '{ important: boolean; }'.


{
  type Simplify<T> = { [KeyType in keyof T]: never };
  type SimpleMerge<Source> = {
    [Key in keyof {}]: never;
  } & Source;
  type Merge<Source> =
    Simplify<SimpleMerge<Source>>;
  type AAA<TTT extends { [Key in keyof TTT]: never; }> = Merge<TTT>;
  type BBBBB<TTTTT extends { a: never } & {[Key in keyof TTTTT]: Key;}> = Merge<TTTTT>;
}