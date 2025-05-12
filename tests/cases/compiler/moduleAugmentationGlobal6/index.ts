// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/moduleAugmentationGlobal6.ts`, Apache-2.0 License

declare global { //~ ERROR: Augmentations for the global scope can only be directly nested in external modules or ambient module declarations.
  interface Array<T> { x }
}