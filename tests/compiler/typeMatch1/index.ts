// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeMatch1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface I { z; }
interface I2 { z; }

declare var x1: { z: number; f(n: number): string; f(s: string): number; }
var x2: { z:number;f:{(n:number):string;(s:string):number;}; } = x1;
declare var i:I;
declare var i2:I2;
var x3:{ z; }= i;
var x4:{ z; }= i2;
var x5:I=i2;

class C { private x; }
class D { private x; }

var x6=new C();
var x7=new D();

x6 = x7;
//~^ ERROR: Type 'D' is not assignable to type 'C'.
x6=C;
//~^ ERROR: Property 'x' is missing.
C==D;
//~^ ERROR: Operator '==' cannot be applied to types 'typeof C' and 'typeof D'.
C==C;

