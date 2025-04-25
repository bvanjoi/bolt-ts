// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/contextualTyping14.ts`, Apache-2.0 License

class foo { public bar:(a:number)=>number = function(a){return a}; }