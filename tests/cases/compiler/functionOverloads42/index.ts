// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionOverloads42.ts`, Apache-2.0 License

function foo(bar:{a:number;}[]):string;
function foo(bar:{a:any;}[]):number;
function foo(bar:{a:any;}[]):any{ return bar }
var x = foo([{a:'s'}]);
var y: number = foo([{a:'s'}]);
var z: string = foo([{a:42}]);

var k: string = x;
//~^ ERROR: Type 'number' is not assignable to type 'string'.