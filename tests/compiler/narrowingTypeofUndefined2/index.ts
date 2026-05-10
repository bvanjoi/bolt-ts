// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/narrowingTypeofUndefined2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

declare function takeArray(arr: Array<unknown>): void;

function fn<T extends Array<unknown> | undefined>(arg: T) {
    if (typeof arg !== "undefined") {
        takeArray(arg);
        const n: Array<unknown> = arg;

        for (const p of arg) {  }
        const m = [...arg];
    }
}
