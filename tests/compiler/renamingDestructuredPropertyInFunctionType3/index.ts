// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/renamingDestructuredPropertyInFunctionType3.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@compiler-options: target=es2015

const sym = Symbol();
type O = Record<symbol, unknown>
type F14 = ({ [sym]: string }: O) => void; // Error
//~^ ERROR: 'string' is an unused renaming of '[computed]'. Did you intend to use it as a type annotation?
type G14 = new ({ [sym]: string }: O) => void; // Error
//~^ ERROR: 'string' is an unused renaming of '[computed]'. Did you intend to use it as a type annotation?
const f13 =  ({ [sym]: string }: O) => { };
function f14 ({ [sym]: string }: O) { };