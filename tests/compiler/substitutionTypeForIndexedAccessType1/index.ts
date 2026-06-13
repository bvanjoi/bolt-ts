// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/substitutionTypeForIndexedAccessType1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
type AddPropToObject<Obj extends object, Prop extends string> = Prop extends keyof Obj
  ? Obj[Prop] extends unknown[]
    ? [...Obj[Prop]]
    : never
  : never