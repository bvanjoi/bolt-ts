// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextuallyTypedParametersWithInitializers4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

declare function test<
  TContext,
  TMethods extends Record<string, (ctx: TContext, ...args: (1 | 2)[]) => unknown>,
>(context: TContext, methods: TMethods): void;

test(
  {
    count: 0,
  },
  {
    checkLimit: (ctx, max = 3) => {},
  },
);
