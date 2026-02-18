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
  type A<T> = { [KeyType in keyof T]: never };
  type B<Source> = {[Key in keyof {}]: never } & Source;
  type C<Source> = A<B<Source>>;
  type AAA<TTT extends { [Key in keyof TTT]: never; }> = C<TTT>;
  type BBBBB<TTTTT extends { a: never } & {[Key in keyof TTTTT]: Key;}> = C<TTTTT>;
}

{
  type T2<E> = keyof {[Key in keyof E as number]: never}
  type T1<C, D extends C> = never;
  type T0<A> = T1<A, T2<A>>;
}