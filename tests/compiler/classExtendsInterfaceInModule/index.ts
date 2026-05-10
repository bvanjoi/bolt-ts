// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classExtendsInterfaceInModule.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace M {
  export interface I1 {}
  export interface I2<T> {}
}
class C1 extends M.I1 {}
//~^ ERROR: Cannot find name 'M'.
class C2<T> extends M.I2<T> {}
//~^ ERROR: Cannot find name 'M'.

namespace Mod {
	export namespace Nested {
		export interface I {}
	}
}

class D extends Mod.Nested.I {}
//~^ ERROR: Cannot find name 'Mod'.
