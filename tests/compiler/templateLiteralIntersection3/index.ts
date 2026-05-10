// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/templateLiteralIntersection3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type Path = string & { _pathBrand: any };
declare const path: Path;

declare const options1: { prop: number; } & { [k: string]: boolean; };

options1[`foo`] = false;

options1[`foo/${path}`] = false;


// Lowercase<`foo/${Path}`> => `foo/${Lowercase<Path>}`
declare const lowercasePath: Lowercase<`foo/${Path}`>;

options1[lowercasePath] = false;
