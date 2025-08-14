// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/cyclicTypeInstantiation.ts`, Apache-2.0 License

function foo<T>() {
  var x: {
      a: T;
      b: typeof x;
  };
  return x;
}

function bar<T>() {
  var x: {
      a: T;
      b: typeof x;
  };
  return x;
}

var a = foo<string>();
var b = bar<string>();
// Relating types of a and b produces instantiations of the cyclic anonymous types in foo and bar
a = b;
