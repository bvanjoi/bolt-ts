// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/voidIsInitialized.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: strict

const x: void = undefined;
const y: void = undefined;

if(typeof x === "undefined") {
    x // no error: assume x2 is initialised
}

if(typeof y !== "undefined") {
    y // no error: do not narrow void
}