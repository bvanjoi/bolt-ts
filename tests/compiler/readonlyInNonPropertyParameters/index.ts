// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/readonlyInNonPropertyParameters.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

// `readonly` won't work outside of property parameters
class X {
	method(readonly x: number) {}
	//~^ ERROR: A parameter property is only allowed in a constructor implementation.
	set x(readonly value: number) {}
	//~^ ERROR: A parameter property is only allowed in a constructor implementation.
}
(readonly x) => 0; //~ERROR: A parameter property is only allowed in a constructor implementation.
// OK to use `readonly` as a name
(readonly) => 0;