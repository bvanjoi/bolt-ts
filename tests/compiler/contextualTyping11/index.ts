// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/contextualTyping11.ts`, Apache-2.0 License

class foo { public bar:{id:number;}[] = [<foo>({})]; }
//~^ ERROR: Property 'id' is missing.