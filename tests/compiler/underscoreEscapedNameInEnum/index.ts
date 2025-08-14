// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/underscoreEscapedNameInEnum.ts`, Apache-2.0 License

enum E {
  "__foo" = 1,
  bar = E["__foo"] + 1
}


let a0: number = E.__foo;
let a1: number = E.bar;
let a2: number = E["__foo"];
let a3: number = E["bar"];