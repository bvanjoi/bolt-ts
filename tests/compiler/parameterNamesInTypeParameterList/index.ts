// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parameterNamesInTypeParameterList.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function f0<T extends typeof a>(a: T) {
  //~^ ERROR: Cannot find name 'a'. 
	a.b;
}

function f1<T extends typeof a>({a}: {a:T}) {
  //~^ ERROR: Cannot find name 'a'. 
	a.b;
}

function f2<T extends typeof a>([a]: T[]) {
  //~^ ERROR: Cannot find name 'a'. 
	a.b;
}

class A {
	m0<T extends typeof a>(a: T) {
  //~^ ERROR: Cannot find name 'a'. 
		a.b
	}
	m1<T extends typeof a>({a}: {a:T}) {
  //~^ ERROR: Cannot find name 'a'. 
		a.b
	}
	m2<T extends typeof a>([a]: T[]) {
  //~^ ERROR: Cannot find name 'a'. 
		a.b
	}
}