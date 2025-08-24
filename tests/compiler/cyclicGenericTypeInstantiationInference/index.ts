// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/cyclicGenericTypeInstantiationInference.ts`, Apache-2.0 License

//@ run-fail

function foo<T>() {
  var z = foo<typeof y>();
  var y: {
      y2: typeof z
  };
  return y;
}


function bar<T>() {
  var z = bar<typeof y>();
  var y: {
      y2: typeof z;
  }
  return y;
}

var a = foo<number>();
var b = bar<number>();

a.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2;
b.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2.y2;

function test<T>(x: typeof a): void { }
test(b);