// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/funduleExportedClassIsUsedBeforeDeclaration.ts`, Apache-2.0 License
//@ run-fail
// interface before module declaration
// uses defined below class in module
// function merged with module

new B.C();