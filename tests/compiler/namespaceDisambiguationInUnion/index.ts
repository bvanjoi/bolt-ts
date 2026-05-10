// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/namespaceDisambiguationInUnion.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace Foo {
  export type Yep = { type: "foo.yep" };
}

namespace Bar {
  export type Yep = { type: "bar.yep" };
}

const x = { type: "wat.nup" };
const val1: Foo.Yep | Bar.Yep = x;
//~^ ERROR: Type '{ type: string; }' is not assignable to type 'Yep | Yep'.

const y = [{ type: "a" }, { type: "b" }];
const val2: [Foo.Yep, Bar.Yep] = y;
//~^ ERROR: Type '{ type: string; }[]' is not assignable to type '[Yep, Yep]'.
