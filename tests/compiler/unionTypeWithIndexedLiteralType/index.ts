// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/unionTypeWithIndexedLiteralType.ts`, Apache-2.0 License

interface I { x: number; }
interface Idx { [index: string]: U; }
type U = Idx | I | "lit";
const u: U = { x: "lit" };