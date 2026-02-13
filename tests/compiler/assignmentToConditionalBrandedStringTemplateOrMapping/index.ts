// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/assignmentToConditionalBrandedStringTemplateOrMapping.ts`, Apache-2.0 License

//@compiler-options: target=es2015

let a: (<T>() => T extends `${'a' & { a: 1 }}` ? 1 : 2) = null!;
let b: (<T>() => T extends `${'a' & { a: 1 }}` ? 1 : 2) = null!;

a = b;

let c: (<T>() => T extends Uppercase<'a' & { a: 1 }> ? 1 : 2) = null!;
let d: (<T>() => T extends Uppercase<'a' & { a: 1 }> ? 1 : 2) = null!;

c = d;
