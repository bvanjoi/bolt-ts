// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/contextualTyping24.ts`, Apache-2.0 License

//@compiler-options: target=es2015
var foo:(a:{():number; (i:number):number; })=>number; foo = function(this: void, a:string){return 5};
//~^ ERROR: Type '(a: string) => number' is not assignable to type '(a: () => number) => number'.