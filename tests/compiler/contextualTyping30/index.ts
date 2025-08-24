// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/contextualTyping30.ts`, Apache-2.0 License

function foo(param:number[]){}; foo([1, "a"]);
//~^ ERROR: Type 'string' is not assignable to type 'number'.