// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/restUnion3.ts`, Apache-2.0 License

//@compiler-options: strict

declare const nullAndUndefinedUnion: null | undefined;
var rest4: { };
var {...rest4 } = nullAndUndefinedUnion;
//~^ ERROR: Rest types may only be created from object types.

declare const unionWithIntersection: ({ n: number } & { s: string }) & undefined;
var rest5: { n: number, s: string };
var {...rest5 } = unionWithIntersection;
//~^ ERROR: Rest types may only be created from object types.