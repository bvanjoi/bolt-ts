// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/implicitAnyGenericTypeInference.ts`, Apache-2.0 License
//@compiler-options: noImplicitAny
//@compiler-options: target=esnext
var c;
c = {
  compareTo: (x, y) => (y)  
};
var r = c.compareTo(1, '');
f1(() => (null));
f2(async () => (null));
f3(function* () {
  yield null;
});
f4(function* () {
  return null;
});
f5(function* () {
  yield null;
});
f6(function* () {
  return null;
});
Promise.resolve().catch((e) => (null));
Promise.resolve().then((v) => (null));