// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/contextualTyping4.ts`, Apache-2.0 License

class foo { public bar:{id:number;} = {id:5, name:"foo"}; }
//~^ ERROR: Object literal may only specify known properties, and 'name' does not exist.
