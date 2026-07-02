// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noCircularitySelfReferentialGetter3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

const a = {
  prop: 42,
  get self() {
    return a;
  },
} satisfies { prop: number; self: any };
