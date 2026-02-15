// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/exportDefaultMissingName.ts`, Apache-2.0 License

export default xyzzy;
//~^ ERROR: Cannot find name 'xyzzy'.

export type A = number;
export namespace A {}

export type B = string;
export namespace B {
  const b: string = '42';
}