// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextuallyTypedByDiscriminableUnion2.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: strict 
//@compiler-options: noEmit

// https://github.com/microsoft/TypeScript/issues/62256

type Identifiable = { id: string };

interface EnableA {
  readonly enableA: true;
  // this introduces a conflicting property with some of the other members of MyComponentProps
  // making relevant final union members reduced nevers
  readonly enableB: true;
}

interface DisableA {
  readonly enableA?: false;
}

interface EnableB {
  readonly enableB?: true;
}

interface DisableB {
  readonly enableB: false;
}

export interface EnableD<I extends Identifiable> {
  readonly enableD: true;
  readonly value: I["id"] | null;
  readonly setItem: (item: I | null) => void;
}

export interface DisableD<I extends Identifiable> {
  readonly enableD: false;
  readonly value: I["id"];
  readonly setItem: (item: I) => void;
}

type MyComponentProps<I extends Identifiable> = (EnableA | DisableA) &
  (EnableB | DisableB) &
  (DisableD<I> | EnableD<I>);

const MyComponent = <I extends Identifiable>(props: MyComponentProps<I>) => {};

declare const item: string | null;

MyComponent({
  enableD: true,
  value: item,
  setItem: (item) => {},
});
