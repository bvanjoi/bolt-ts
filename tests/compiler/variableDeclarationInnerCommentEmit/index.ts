// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/variableDeclarationInnerCommentEmit.ts`, Apache-2.0 License

var a = /*some comment*/ null;
var b /*some comment*/ = null;
var /*some comment*/ c = null;

// no space
var a=/*some comment*/null;