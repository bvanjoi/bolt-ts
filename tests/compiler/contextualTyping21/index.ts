// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/contextualTyping21.ts`, Apache-2.0 License

var foo:{id:number;}[] = [{id:1}]; foo = [{id:1}, 1];
//~^ ERROR: Type 'number' is not assignable to type '{ id: number; }'.