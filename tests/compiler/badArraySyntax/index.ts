// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/badArraySyntax.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class Z {
  public x = "";
}

var a1: Z[] = [];
var a2 = new Z[];
//~^ ERROR: An element access expression should take an argument.
var a3 = new Z[]();
//~^ ERROR: An element access expression should take an argument.
var a4: Z[] = new Z[];
//~^ ERROR: An element access expression should take an argument.
var a5: Z[] = new Z[]();
//~^ ERROR: An element access expression should take an argument.
var a6: Z[][] = new   Z     [      ]   [  ];
//~^ ERROR: An element access expression should take an argument.
//~| ERROR: An element access expression should take an argument.
