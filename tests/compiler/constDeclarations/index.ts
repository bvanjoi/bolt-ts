// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constDeclarations.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@compiler-options: strict=false
//@compiler-options: declaration

// No error
const c1 = false;
const c2: number = 23;
const c3 = 0, c4 :string = "", c5 = null;


for(const c4 = 0; c4 < 9; ) { break; }


for(const c5 = 0, c6 = 0; c5 < c6; ) { break; }

declare namespace A {
  const a: number;
}