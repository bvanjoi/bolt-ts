// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/functionOverloads43.ts`, Apache-2.0 License

function foo(bar: { a:number }[]): number;
function foo(bar: { a:string }[]): string;
function foo([x]: { a:number | string }[]): string | number {
    if (x) {
        return x.a;
    }
    
    return undefined;
}

var x = foo([{a: "str"}]);
var y = foo([{a: 100}]);