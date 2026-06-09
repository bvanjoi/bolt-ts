// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mergedDeclarationExports.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

// OK -- one is type, one is value
interface b {}
export const b = 1;

// OK -- one is a type, one is a namespace, one is a value.
type t = 0;
namespace t { interface I {} }
export const t = 0;

// Should get errors if they have some meaning in common.

// both types
interface c {}
//~^ ERROR: Individual declarations in merged declaration 'c' must be all exported or all local.
export interface c {}
//~^ ERROR: Individual declarations in merged declaration 'c' must be all exported or all local.

// both types (class is also value, but that doesn't matter)
interface d {}
//~^ ERROR: Individual declarations in merged declaration 'd' must be all exported or all local.
export class d {}
//~^ ERROR: Individual declarations in merged declaration 'd' must be all exported or all local.

// both namespaces
namespace N { }
//~^ ERROR: Individual declarations in merged declaration 'N' must be all exported or all local.
export namespace N {}
//~^ ERROR: Individual declarations in merged declaration 'N' must be all exported or all local.
