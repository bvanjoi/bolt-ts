// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/bindingPatternContextualTypeDoesNotCauseWidening.ts`, Apache-2.0 License

declare function pick<O, T extends keyof O>(keys: T[], obj?: O): Pick<O, T>;
const _    = pick(['b'], { a: 'a', b: 'b' }); // T: "b"
const {  } = pick(['b'], { a: 'a', b: 'b' }); // T: "b" | "a" ??? (before fix)

const a0: { b: 'a' } = pick(['b'], { a: 'a', b: 'b' }); // T: "b"
//~^ ERROR: Type 'Pick' is not assignable to type '{ b: "a"; }'.