// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualReturnTypeOfIIFE2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[esnext]
//@compiler-options: noImplicitAny
//@run-fail

declare namespace app {
  function foo(): void;
}

app.foo.bar = (function () {
  const someFun = (arg: number) => {};
  return { someFun };
})();

app.foo.bar.someFun(1);
