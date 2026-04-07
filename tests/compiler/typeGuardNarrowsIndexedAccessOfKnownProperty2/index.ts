// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeGuardNarrowsIndexedAccessOfKnownProperty2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

const foo: { key?: number } = {};
const key = 'key' as const;

if (foo[key]) {
    foo[key]; // number
    foo.key;  // number
    let a0: number = foo[key];
    let a1: string = foo[key];
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
    let b0: number = foo.key;
    let b1: string = foo.key;
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
}

function s(a: {b: 'A'}) {
    let b: 'B' = a.b;
    //~^ ERROR: Type '"A"' is not assignable to type '"B"'.
}

type Foo = {
	a: string;
	b?: undefined 
} | {
	a?: undefined;
	b: string;
}
function f0(foo: Foo) {
	if (typeof foo.a === 'string') {
		return;
	}
	let b: number = foo.b;
    //~^ ERROR: Type 'string' is not assignable to type 'number'.
}