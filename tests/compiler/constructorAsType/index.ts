// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/constructorAsType.ts`, Apache-2.0 License

var Person:new () => {name: string;} = function () {return {name:"joe"};};
//~^ ERROR: Type '() => { name: string; }' is not assignable to type 'new () => { name: string; }'

var Person2:{new() : {name:string;};};

Person = Person2;