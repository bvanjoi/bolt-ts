// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeAliasExport.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

declare module "a" {
  export default undefined
  export var a;
  export type a = typeof a;
}


declare module "b" {
  export default null;
  //~^ ERROR: The expression of an export assignment must be an identifier or qualified name in an ambient context.
}