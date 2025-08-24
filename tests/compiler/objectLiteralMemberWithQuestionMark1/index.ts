// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/objectLiteralMemberWithQuestionMark1.ts`, Apache-2.0 License

var v = { foo?() { } }
//~^ ERROR: An object member cannot be declared optional