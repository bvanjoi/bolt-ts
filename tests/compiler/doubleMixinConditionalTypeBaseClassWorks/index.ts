// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/doubleMixinConditionalTypeBaseClassWorks.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type Constructor = new (...args: any[]) => {};

const Mixin1 = <C extends Constructor>(Base: C) => class extends Base { private _fooPrivate: {}; }
//~^ ERROR: Property '_fooPrivate' has no initializer and is not definitely assigned in the constructor.
//~| ERROR: Property '_fooPrivate' has no initializer and is not definitely assigned in the constructor.

type FooConstructor = typeof Mixin1 extends (a: Constructor) => infer Cls ? Cls : never;
const Mixin2 = <C extends FooConstructor>(Base: C) => class extends Base {};

class C extends Mixin2(Mixin1(Object)) {}
