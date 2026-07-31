// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/m7Bugs.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// scenario 1
interface ISomething {
   something: number;
}

var s: ISomething = <ISomething>({ });


// scenario 2
interface A { x: string; }

interface B extends A { }

var x: B = <B>{ };

class C1 {
	public x: string;
  //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor. 
}

class C2 extends C1 {}

var y1: C1 = new C2();
var y2: C1 = <C1> new C2();
var y3: C1 = <C1> {};

