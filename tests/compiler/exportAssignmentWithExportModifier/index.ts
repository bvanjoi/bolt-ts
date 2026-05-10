// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/exportAssignmentWithDeclareModifier.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

var x;
export export = x;
//~^ ERROR: An export assignment cannot have modifiers.