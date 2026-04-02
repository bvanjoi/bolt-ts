// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/unionTypeWithRecursiveSubtypeReduction3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare var a27: { prop: number } | { prop: T27 };
type T27 = typeof a27;

declare var b: T27;
var s: string = b;
//~^ ERROR: Type '{ prop: number; } | { prop: ...; }' is not assignable to type 'string'.
