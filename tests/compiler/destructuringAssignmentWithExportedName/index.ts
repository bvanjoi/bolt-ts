// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructuringAssignmentWithDefault2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

export let exportedFoo: any;
let nonexportedFoo: any;

// sanity checks
exportedFoo = null;
nonexportedFoo = null;

if (null as any) {
  //~^ ERROR: This kind of expression is always falsy.
    ({ exportedFoo, nonexportedFoo } = null as any);
}
else if (null as any) {
  //~^ ERROR: This kind of expression is always falsy.
	({ foo: exportedFoo, bar: nonexportedFoo } = null as any);
}
else if (null as any) {
  //~^ ERROR: This kind of expression is always falsy.
	({ foo: { bar: exportedFoo, baz: nonexportedFoo } } = null as any);
}
else if (null as any) {
  //~^ ERROR: This kind of expression is always falsy.
	([exportedFoo, nonexportedFoo] = null as any);
}
else {
	([[exportedFoo, nonexportedFoo]] = null as any);
}

export { nonexportedFoo };
export { exportedFoo as foo, nonexportedFoo as nfoo };