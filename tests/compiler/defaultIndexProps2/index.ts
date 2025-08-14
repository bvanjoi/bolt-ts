// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/defaultIndexProps2.ts`, Apache-2.0 License

class Foo {
	public v = "Yo";
}

var f = new Foo();

// WScript.Echo(f[0]);

var o = {v:"Yo2"};

// WScript.Echo(o[0]);

1[0];
var q = "s"[0];
