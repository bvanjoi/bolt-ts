
// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeArgumentInferenceWithRecursivelyReferencedTypeAliasToTypeLiteral01.ts`, Apache-2.0 License
//@ run-fail
var nodes;
nodes.map((n) => n.name);