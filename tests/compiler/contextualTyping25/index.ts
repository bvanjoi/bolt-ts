// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/contextualTyping25.ts`, Apache-2.0 License

//@compiler-options: target=es2015
function foo(param:{id:number;}){}; foo(<{id:number;}>({}));