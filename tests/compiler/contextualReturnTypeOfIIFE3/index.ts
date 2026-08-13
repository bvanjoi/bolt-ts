// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualReturnTypeOfIIFE3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[esnext]
//@compiler-options: noImplicitAny

declare namespace app {
  var foo: {
    bar: {
      someFun: (arg: number) => void;
    };
  };
}

app.foo.bar = (function () {
  return { someFun(arg) {
    let s: string = arg;
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
  } };
})();

app.foo.bar.someFun(1);
