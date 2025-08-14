// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/assignmentCompatInterfaceWithStringIndexSignature.ts`, Apache-2.0 License

interface IHandler {
  (e): boolean;
}

interface IHandlerMap {
  [type: string]: IHandler;
}

class Foo {
  public Boz(): void { }
}

function Biz(map: IHandlerMap) { }

Biz(new Foo());
//~^ ERROR: Argument of type 'Foo' is not assignable to parameter of type 'IHandlerMap'.