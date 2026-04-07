// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/contextualTyping23.ts`, Apache-2.0 License

//@compiler-options: target=es2015
var foo:(a:{():number; (i:number):number; })=>number; foo = function(a){return 5};