// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parenthesisDoesNotBlockAliasSymbolCreation.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

export type InvalidKeys<K extends string|number|symbol> = { [P in K]? : never };
export type InvalidKeys2<K extends string|number|symbol> = (
    { [P in K]? : never }
);

export type A<T> = (
    T & InvalidKeys<"a">
);
export type A2<T> = (
    T & InvalidKeys2<"a">
);

export const a = null as A<{ x : number }>;
//~^ ERROR: Conversion of type 'null' to type 'A<{ x: number; }>' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.
export const a2 = null as A2<{ x : number }>;
//~^ ERROR: Conversion of type 'null' to type 'A2<{ x: number; }>' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.
export const a3 = null as { x : number } & InvalidKeys<"a">;
//~^ ERROR: Conversion of type 'null' to type '{ x: number; } & InvalidKeys<"a">' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.
export const a4 = null as { x : number } & InvalidKeys2<"a">;
//~^ ERROR: Conversion of type 'null' to type '{ x: number; } & InvalidKeys2<"a">' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.
