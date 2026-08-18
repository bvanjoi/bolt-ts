// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/letDeclarations.ts`, Apache-2.0 License

//@compiler-options: target=ES6
//@compiler-options: declaration


let l1;
let l2: number;
let l3, l4, l5 :string, l6;

let l7 = false;
let l8: number = 23;
let l9 = 0, l10 :string = "", l11 = null;

for(let l11 in {}) { }

for(let l12 = 0; l12 < 9; l12++) { }
