// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/untypedFunctionCallsWithTypeParameters1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

// none of these function calls should be allowed
var x = function () { return; };
var r1 = x<number>();
//~^ ERROR: Expected 0 type arguments, but got 1.
var y: any = x;
var r2 = y<string>();
//~^ ERROR: Untyped function calls may not accept type arguments.

declare var c: Function;
var r3 = c<number>(); // should be an error
//~^ ERROR: Untyped function calls may not accept type arguments.

class C implements Function {
  //~^ ERROR: Type 'C<C>' is missing the following properties from type 'Function': apply, call, and 3 more.
    prototype = null;
    length = 1;
    arguments = null;
    caller = () => { };
}

declare var c2: C;
var r4 = c2<number>(); // should be an error
//~^ ERROR: This expression is not callable.

class C2 extends Function { } // error
declare var c3: C2;
var r5 = c3<number>(); // error
//~^ ERROR: Untyped function calls may not accept type arguments.

interface I {
    (number): number;
}
declare var z: I;
var r6 = z<string>(1); // error
//~^ ERROR: Expected 0 type arguments, but got 1.

interface callable2<T> {
    (a: T): T;
}

declare var c4: callable2<number>;
c4<number>(1);
//~^ ERROR: Expected 0 type arguments, but got 1.
interface callable3<T> {
    (a: T): T;
}

declare var c5: callable3<number>;
c5<string>(1); // error
//~^ ERROR: Expected 0 type arguments, but got 1.

