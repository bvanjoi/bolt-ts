// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/checkSuperCallBeforeThisAccessing3.ts`, Apache-2.0 License
class Based {}
class Derived extends Based {
  x;
  constructor() {class innver {
      y;
      constructor() {this.y = true;}
    }
    super();this.x = 10;
    var that = this;}
}