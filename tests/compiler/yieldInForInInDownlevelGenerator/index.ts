// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/yieldInForInInDownlevelGenerator.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: lib=[esnext]

// https://github.com/microsoft/TypeScript/issues/49808
function* gen() {
  var obj: any = { foo: 1, bar: 2 };
  for (var key in obj) {
      yield key;
      delete obj.bar;
  }
}