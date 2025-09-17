// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/transitiveTypeArgumentInference1.ts`, Apache-2.0 License

interface I1<T, U> {
	m(value: T): U;
}

var i: I1<boolean, string> = null;

class C<T> {
	constructor(p: I1<boolean, T>) {
	}
}

var c = new C(i);
