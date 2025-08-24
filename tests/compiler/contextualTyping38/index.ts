// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/contextualTyping38.ts`, Apache-2.0 License

var foo = <{ (): number; }> function(a) { return a };
