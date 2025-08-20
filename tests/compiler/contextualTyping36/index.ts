// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/contextualTyping36.ts`, Apache-2.0 License

var foo = <{ id: number; }[]>[{ id: 4 }, <{ id: number; }>({  })];