// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructuringFromUnionSpread.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface A { a: string }
interface B { b: number }

declare const x: A | B;
const { a } = { ...x } // error
//~^ ERROR: Property '"a"' does not exist on type '{ a: string; } | { b: number; }'.
