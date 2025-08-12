// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/recursiveClassInstantiationsWithDefaultConstructors.ts`, Apache-2.0 License

module TypeScript2 {
  export class MemberName {
      public prefix: string = "";
  }
  export class MemberNameArray extends MemberName {
  }
}

var a = new TypeScript2.MemberNameArray()
var b: number = a.prefix;
//~^ ERROR: Type 'string' is not assignable to type 'number'.