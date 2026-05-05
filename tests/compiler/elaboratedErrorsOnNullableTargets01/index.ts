// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/elaboratedErrorsOnNullableTargets01.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

export declare let x: null | { foo: { bar: string | null } | undefined } | undefined;
export declare let y: { foo: { bar: number | undefined } };

x = y;
//~^ ERROR: Type '{ foo: { bar: undefined | number; }; }' is not assignable to type 'undefined | null | { foo: undefined | { bar: null | string; }; }'.

y = x;
//~^ ERROR: Type 'undefined | null | { foo: undefined | { bar: null | string; }; }' is not assignable to type '{ foo: { bar: undefined | number; }; }
