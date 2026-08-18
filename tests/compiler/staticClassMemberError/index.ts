// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/staticClassMemberError.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C {
	static s;
	public a() {
		s = 1;
    //~^ ERROR: Cannot find name 's'.
	}
}

// just want to make sure this one doesn't crash the compiler
function Foo();
//~^ ERROR: Function implementation is missing or not immediately following the declaration.
//~| ERROR: Function with bodies can only merge with classes that are ambient.
class Foo {
  //~^ ERROR: Class declaration cannot implement overload list for 'Foo'.
 static bar;
}