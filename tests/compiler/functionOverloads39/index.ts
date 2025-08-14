// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads39.ts`, Apache-2.0 License

function foo(bar:{a:number;}[]):string;
function foo(bar:{a:boolean;}[]):number;
function foo(bar:{a:any;}[]):any{ return bar }
var x = foo([{a:true}]);

var y: string = foo([{a:1}]);
var z: number = foo([{a:false}]);
var k: string = x;
//~^ ERROR: Type 'number' is not assignable to type 'string'.