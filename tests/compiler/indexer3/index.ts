// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexer3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var dateMap: { [x: string]: Date; } = {}
var r: Date = dateMap["hello"] // result type includes indexer using BCT