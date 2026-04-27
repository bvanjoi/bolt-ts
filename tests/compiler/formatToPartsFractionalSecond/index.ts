// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/checkDestructuringShorthandAssigment2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noEmit

// GH #38175 -- should not crash while checking

let o: any, k: any;
let { x } = { x: 1, ...o, [k]: 1 };
