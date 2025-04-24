// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/contextualTyping15.ts`, Apache-2.0 License

class foo { public bar: { (): number; (i: number): number; } = function() { return 1 }; }