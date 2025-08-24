// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/lift.ts`, Apache-2.0 License

class B {
  constructor(public y:number) {
  }
  public ll:number;  // to be shadowed
}

class C extends B {
  constructor(y:number,z:number,w:number) {
      super(y)
      var x=10+w;
      var ll=x*w;
  }

  public liftxyz () { return x+z+this.y; }
  //~^ ERROR: Cannot find name 'x'.
  //~| ERROR: Cannot find name 'z'.
  public liftxylocllz () { return x+z+this.y+this.ll; }
  //~^ ERROR: Cannot find name 'x'.
  //~| ERROR: Cannot find name 'z'.
}
