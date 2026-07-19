// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/defaultIndexProps2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class Foo {
	public v = "Yo";
}

var f = new Foo();

// WScript.Echo(f[0]);

var o = {v:"Yo2"};

// WScript.Echo(o[0]);

1[0];
var q = "s"[0];
