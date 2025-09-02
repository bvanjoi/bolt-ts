// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/contextualTyping26.ts`, Apache-2.0 License

function foo(param:{id:number;}){}; foo(<{id:number;}>({}));