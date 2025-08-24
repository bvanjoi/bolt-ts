// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/contextualTyping40.ts`, Apache-2.0 License

var foo = <{():number; (i:number):number; }> function(){return 1;};