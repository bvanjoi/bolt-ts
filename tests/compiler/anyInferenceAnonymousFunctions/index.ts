// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/anyInferenceAnonymousFunctions.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var paired: any[];

paired.reduce(function (a1, a2) {
  //~^ ERROR: Variable 'paired' is used before being assigned.

    return a1.concat({});

} , []);

paired.reduce((b1, b2) => {
  //~^ ERROR: Variable 'paired' is used before being assigned.

    return b1.concat({});
} , []);

paired.reduce((b3, b4) => b3.concat({}), []);
  //~^ ERROR: Variable 'paired' is used before being assigned.
paired.map((c1) => c1.count);
  //~^ ERROR: Variable 'paired' is used before being assigned.
paired.map(function (c2) { return c2.count; });
  //~^ ERROR: Variable 'paired' is used before being assigned.