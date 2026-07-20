// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleOuterQualification.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

declare namespace outer {
  interface Beta { }
  namespace inner {
    // .d.ts emit: should be 'extends outer.Beta'
    export interface Beta extends outer.Beta { }
  }
}
