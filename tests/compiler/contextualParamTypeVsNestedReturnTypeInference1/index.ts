// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualParamTypeVsNestedReturnTypeInference1.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: strict
//@compiler-options: noEmit

interface Effect<A> {
  _A: A;
}
  
declare function effectGen<AEff>(f: () => AEff): Effect<AEff>;

declare function effectFn<AEff, Args extends Array<any>>(
  body: (...args: Args) => unknown,
): (...args: Args) => Effect<AEff>;
  
declare function layerEffect<S>(tag: Tag<S>, effect: Effect<S>): unknown;
  
interface Tag<Type> {
  _Type: Type;
}
  
declare const Foo: Tag<{
  fn: (a: string) => unknown;
}>;
  
layerEffect(
  Foo,
  effectGen(function () {
    return {
      fn: effectFn(function (a) {
        a; // string
      }),
    };
  }),
);
