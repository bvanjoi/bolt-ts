// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/contextualTypeSelfReferencing.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

// repro from https://github.com/microsoft/TypeScript/issues/54048

type narrow<def> = def extends string
  ? def
  : def extends [unknown, ...unknown[]]
  ? def
  : {
      [k in keyof def]: narrow<def[k]>;
    };

declare const parse: <def>(def: narrow<def>) => def;

const result = parse([{ a: "foo" }]);