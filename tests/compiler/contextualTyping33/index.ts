// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/contextualTyping33.ts`, Apache-2.0 License

function foo(param: {():number; (i:number):number; }[]) { }; foo([function(){return 1;}, function(){return "foo"}]);
//~^ ERROR: Type '() => string' is not assignable to type '() => number'.