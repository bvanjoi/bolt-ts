// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/discriminateWithMissingProperty.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

const a: number = new Uint8Array([1, 2, 3]);
//~^ ERROR: Type 'Uint8Array<ArrayBuffer>' is not assignable to type 'number'.

type Arg = {
    mode: "numeric",
    data: number,
} | {
    mode: "alphabetic",
    data: string,
} | {
    data: string | Uint8Array;
}

declare function foo(arg: Arg): void;
foo({ mode: "numeric", data: new Uint8Array([30]) }); // Should error