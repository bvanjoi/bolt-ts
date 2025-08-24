// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads36.ts`, Apache-2.0 License

function foo(bar:{a:number;}):number;
function foo(bar:{a:string;}):string;
function foo(bar:{a:any;}):any{ return bar }
var x = foo({a:'foo'});
var y: string = foo({a:'foo'});
var z: number = foo({a:42});

var k: number = x;
//~^ ERROR: Type 'string' is not assignable to type 'number'.