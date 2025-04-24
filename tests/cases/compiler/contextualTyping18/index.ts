// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/contextualTyping18.ts`, Apache-2.0 License

var foo: {id:number;} = <{id:number;}>({ }); foo = {id: 5};