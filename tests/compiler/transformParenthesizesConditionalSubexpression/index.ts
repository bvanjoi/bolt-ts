// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/transformParenthesizesConditionalSubexpression.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

var K = 'k'
var a = { p  : (true ? { [K] : 'v'}        : null) }
var b = { p  : (true ? { [K] : 'v'} as any : null) }
