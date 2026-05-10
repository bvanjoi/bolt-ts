// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nestedGenericConditionalTypeWithGenericImportType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

type A = import('./name').Name<string>;
type T<X> = any extends ((any extends any ? any : string) extends any ? import("./name").Name<X> : any)
  ? any
  : any;
