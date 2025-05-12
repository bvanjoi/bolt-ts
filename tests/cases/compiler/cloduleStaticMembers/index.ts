// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/cloduleStaticMembers.ts`, Apache-2.0 License

class Clod {
  private static x = 10;
  public static y = 10;
}
module Clod {
  var p = Clod.x;
  //~^ ERROR: Property 'x' is private and only accessible within class.
  var q = x;
  //~^ ERROR: Cannot find name 'x'.

  var s = Clod.y;
  var t = y; 
  //~^ ERROR: Cannot find name 'y'.
}
