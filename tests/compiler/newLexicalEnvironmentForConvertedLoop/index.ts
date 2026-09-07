// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/newLexicalEnvironmentForConvertedLoop.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: noImplicitAny

function baz(x: any) {
  return [[x, x]];
}

function foo(set: any) {
  for (const [value, i] of baz(set.values)) {
    const bar: any = [];
    (() => bar);

    set.values.push(...[]);
  }
};