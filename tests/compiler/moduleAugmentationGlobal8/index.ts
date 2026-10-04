// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleAugmentationGlobal8.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: module=esnext

namespace A {
    declare global {
      //~^ ERROR: Augmentations for the global scope can only be directly nested in external modules or ambient module declarations.
        interface Array<T> { x }
    }
}
export {}
