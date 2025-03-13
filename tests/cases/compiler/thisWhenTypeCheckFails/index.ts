// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisWhenTypeCheckFails.ts`, Apache-2.0 License

class c {
  public n() {
      var k = () => {
          var s: string = this.n();
          //~^ ERROR: Type 'void' is not assignable to type 'string'.
      }
  }    
}
