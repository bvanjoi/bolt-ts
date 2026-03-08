// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/resolveTypeAliasWithSameLetDeclarationName1.ts`, Apache-2.0 License

class C { }
type baz = C;
let baz: baz;


type A = number;
declare function A(): A;

type B<T> = T;
declare function B(): B<string>;

type D<T> = T | number;
declare function D(): D<string>;

type E = number | string;
declare function E(): E;