// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/stringMappingAssignability.ts`, Apache-2.0 License

//@compiler-options: strict

const x: Uppercase<string> = 42;
//~^ ERROR: Type 'number' is not assignable to type 'Uppercase'.
const y: Uppercase<string> = { foo: "bar" };
//~^ ERROR: Type '{ foo: string; }' is not assignable to type 'Uppercase'.

let b0: Uppercase<string> = '42'
let b1: Uppercase<string> = '42' as `${Uppercase<string>}`;
let b2: Uppercase<string> = '42' as Uppercase<string>;

{
  type Defined<T> = Exclude<{[Key in keyof T]: Key}[keyof T], undefined>
  type OnlyMap<T> = {[KeyType in keyof T]: T[KeyType]};
  type A<T> = OnlyMap<{[Key in keyof Pick<T, Defined<T>>]: Key;}>;
}

{
  type O = {
    b?: boolean;	
  }
  type B<T extends boolean> = T;
  type A<Options extends Required<O>> = B<Options['b']>;
}
