// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/defaultArgsInOverloads.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function fun(a: string);
function fun(a = 3);
//~^ ERROR: A parameter initializer is only allowed in a function or constructor implementation.
function fun(a = null) { }

class C {
	fun(a: string);
	fun(a = 3);
//~^ ERROR: A parameter initializer is only allowed in a function or constructor implementation.
	fun(a = null) { }
	static fun(a: string);
	static fun(a = 3);
//~^ ERROR: A parameter initializer is only allowed in a function or constructor implementation.
	static fun(a = null) { }
}

interface I {
    fun(a: string);
    fun(a = 3);
//~^ ERROR: A parameter initializer is only allowed in a function or constructor implementation.
}

var f: (a = 3) => number;
//~^ ERROR: A parameter initializer is only allowed in a function or constructor implementation.