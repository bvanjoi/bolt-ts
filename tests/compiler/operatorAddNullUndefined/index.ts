// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/operatorAddNullUndefined.ts`, Apache-2.0 License

//@compiler-options: target=es2015

enum E { x }
var x1 = null + null;
//~^ ERROR: Operator '+' cannot be applied to types 'null' and 'null'.
var x2 = null + undefined;
//~^ ERROR: Operator '+' cannot be applied to types 'null' and 'undefined'.
var x3 = undefined + null;
//~^ ERROR: Operator '+' cannot be applied to types 'undefined' and 'null'.
var x4 = undefined + undefined;
//~^ ERROR: Operator '+' cannot be applied to types 'undefined' and 'undefined'.
var x5 = 1 + null;
//~^ ERROR: Operator '+' cannot be applied to types '1' and 'null'.
var x6 = 1 + undefined;
//~^ ERROR: Operator '+' cannot be applied to types '1' and 'undefined'.
var x7 = null + 1;
//~^ ERROR: Operator '+' cannot be applied to types 'null' and '1'.
var x8 = undefined + 1;
//~^ ERROR: Operator '+' cannot be applied to types 'undefined' and '1'.
var x9 = "test" + null;
var x10 = "test" + undefined;
var x11 = null + "test";
var x12 = undefined + "test";
var x13 = null + E.x
//~^ ERROR: Operator '+' cannot be applied to types 'null' and 'E.x'.
var x14 = undefined + E.x
//~^ ERROR: Operator '+' cannot be applied to types 'undefined' and 'E.x'.
var x15 = E.x + null
//~^ ERROR: Operator '+' cannot be applied to types 'E.x' and 'null'.
var x16 = E.x + undefined
//~^ ERROR: Operator '+' cannot be applied to types 'E.x' and 'undefined'.