// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericFunctions2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

declare function map <T, U > (items: T[], f: (x: T) => U): U[];

var myItems: string[];
var lengths = map(myItems, x => x.length);
//~^ ERROR: Variable 'myItems' is used before being assigned.
//~| ERROR: Variable 'myItems' is used before being assigned.
//~| ERROR: Variable 'myItems' is used before being assigned.
//~| ERROR: Variable 'myItems' is used before being assigned.
