// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/genericNewInterface.ts`, Apache-2.0 License

function createInstance<T>(ctor: new (s: string) => T): T {
  return new ctor(42); 
  //~^ ERROR:Argument of type 'number' is not assignable to parameter of type 'string'
}
                 
interface INewable<T> {
  new (param: string): T;
}

function createInstance2<T>(ctor: INewable<T>): T {
  return new ctor(1024);
  //~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'
}