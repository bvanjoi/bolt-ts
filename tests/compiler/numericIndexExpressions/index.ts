// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/numericIndexExpressions.ts`, Apache-2.0 License

interface Numbers1 {
    1: string;
}
interface Strings1 {
    '1': string;
}
 
 
var x: Numbers1;
x[1] = 4; // error
//~^ ERROR: Type 'number' is not assignable to type 'string'.
x['1'] = 4; // error
//~^ ERROR: Type 'number' is not assignable to type 'string'.

var y: Strings1;
y['1'] = 4; // should be error
//~^ ERROR: Type 'number' is not assignable to type 'string'.
y[1] = 4; // should be error
//~^ ERROR: Type 'number' is not assignable to type 'string'.
