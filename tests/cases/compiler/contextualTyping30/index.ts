// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/contextualTyping30.ts`, Apache-2.0 License

function foo(param:number[]){}; foo([1, "a"]);
//~^ ERROR: Argument of type 'number | string[]' is not assignable to parameter of type 'number[]'.