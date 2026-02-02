// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/contextualTypeAny.ts`, Apache-2.0 License

var x: any;

var obj: { [s: string]: number } = { p: "", q: x };
//~^ ERROR: Type 'string' is not assignable to type 'number'.

var arr: number[] = ["", x];