// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/objectLiteralFreshnessWithSpread.ts`, Apache-2.0 License

let x = { b: 1, extra: 2 }
let xx: { a, b }  = { a: 1, ...x, z: 3 } // error for 'z', no error for 'extra'
//~^ ERROR: Object literal may only specify known properties, and 'z' does not exist.