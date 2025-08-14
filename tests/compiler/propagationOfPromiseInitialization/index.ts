// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/propagationOfPromiseInitialization.ts`, Apache-2.0 License

interface IPromise<T> {
  then<TResult>(successCallback: (promiseValue: T) => TResult, errorCallback?: (reason: any) => TResult): IPromise<TResult>;
}

var foo: IPromise<number>;
foo.then((x) => {
  let y: string = x;
  //~^ ERROR: Type 'number' is not assignable to type 'string'.
  return "asdf";
}).then((x) => {
  let y: number = x;
  //~^ ERROR: Type 'string' is not assignable to type 'number'.
  x.length;
  return 123;
});
