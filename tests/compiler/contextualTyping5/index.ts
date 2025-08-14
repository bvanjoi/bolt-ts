// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/contextualTyping5.ts`, Apache-2.0 License

class foo { public bar:{id:number;} = { }; }
//~^ ERROR: Property 'id' is missing.