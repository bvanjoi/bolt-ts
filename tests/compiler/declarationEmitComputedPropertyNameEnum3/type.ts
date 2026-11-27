export namespace Foo {
  export enum Enum {
    A = "a",
    B = "b",
  }
}
export type Type = { x?: { [Foo.Enum]: 0 } };
//~^ ERROR: A computed property name must be of type 'string', 'number', 'symbol' or 'any'.