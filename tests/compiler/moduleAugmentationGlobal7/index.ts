// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleAugmentationGlobal7.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: strict=false

namespace A {
    declare global {
      //~^ ERROR: Augmentations for the global scope can only be directly nested in external modules or ambient module declarations.
        interface Array<T> { x }
    }
}
