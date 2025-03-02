// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/defaultIndexProps1.ts`, Apache-2.0 License

class Foo {
	public v = "Yo";
}

var f = new Foo();

var q = f["v"];

var o = {v:"Yo2"};

var q2 = o["v"];
