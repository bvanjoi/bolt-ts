// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/staticVisibility.ts`, Apache-2.0 License

class C1 {
    
  p: any;
  static s: any;

  constructor() {
      var v = 0;

      s = 1; // should be error
      //~^ ERROR: Cannot find name 's'.
      C1.s = 1; // should be ok

      b(); // should be error
      //~^ ERROR: Cannot find name 'b'.
      C1.b(); // should be ok
  }

  static b() {
      v = 1; // should be error
      //~^ ERROR: Cannot find name 'v'.
      this.p = 0; // should be error
      //~^ ERROR: Property 'p' does not exist on type 'typeof C1'.
      C1.s = 1; // should be ok
  }
}

class C2 {

barback:string = "";




static get Bar() {return "bar";} // ok

static set Bar(bar:string) {barback = bar;} // not ok
                          //~^ ERROR: Cannot find name 'barback'.
}

