// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/contextualTyping22.ts`, Apache-2.0 License

var foo:(a:number)=>number = function(a){return a}; foo = function(b){return b};