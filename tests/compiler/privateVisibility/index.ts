// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/privateVisibility.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Foo {
	public pubMeth() {this.privMeth();}
	private privMeth() {}
	public pubProp = 0;
	private privProp = 0;
}

var f = new Foo();
f.privMeth(); // should not work
//~^ ERROR: Property 'privMeth' is private and only accessible within class 'Foo'.
f.privProp; // should not work
//~^ ERROR: Property 'privProp' is private and only accessible within class 'Foo'.

f.pubMeth(); // should work
f.pubProp; // should work

namespace M {
    export class C { public pub = 0; private priv = 1; }
    export var V = 0;
}


var c = new M.C();

c.pub; // should work
c.priv; // should not work
//~^ ERROR: Property 'priv' is private and only accessible within class 'M.C'.