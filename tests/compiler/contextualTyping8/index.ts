// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualTyping8.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var foo:{id:number;}[] = [<{id:number;}>({})];