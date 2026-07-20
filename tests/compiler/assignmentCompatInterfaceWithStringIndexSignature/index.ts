// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentCompatInterfaceWithStringIndexSignature.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

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
