// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/contextualTyping9.ts`, Apache-2.0 License

var foo:{id:number;}[] = [{id:1}, {id:2, name:"foo"}];
//~^ ERROR: Object literal may only specify known properties, and 'name' does not exist.