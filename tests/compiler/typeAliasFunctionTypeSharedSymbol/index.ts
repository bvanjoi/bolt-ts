// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeAliasFunctionTypeSharedSymbol.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// Repro from comment in #21496

function Mixin<TBase extends {new (...args: any[]): {}}>(Base: TBase) {
    return class extends Base {
    };
}

type Mixin = ReturnTypeOf<typeof Mixin>

type ReturnTypeOf<V> = V extends (...args: any[])=>infer R ? R : never;

type Crashes = number & Mixin;
