// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/collisionArgumentsArrowFunctions.ts`, Apache-2.0 License

var f1 = (i: number, ...arguments) => { //arguments is error
//~^ ERROR: Duplicate identifier 'arguments'. Compiler uses 'arguments' to initialize rest parameters.
  //~| ERROR: Invalid use of 'arguments' in strict mode.
  var arguments: any[]; // no error
  //~^ ERROR: Invalid use of 'arguments' in strict mode.
}
var f12 = (arguments: number, ...rest) => { //arguments is error
//~^ ERROR: Duplicate identifier 'arguments'. Compiler uses 'arguments' to initialize rest parameters.
  //~| ERROR: Invalid use of 'arguments' in strict mode.
  var arguments = 10; // no error
  //~^ ERROR: Invalid use of 'arguments' in strict mode.
}
var f1NoError = (arguments: number) => { // no error
  //~^ ERROR: Invalid use of 'arguments' in strict mode.
  var arguments = 10; // no error
  //~^ ERROR: Invalid use of 'arguments' in strict mode.
}

var f2 = (...restParameters) => {
  var arguments = 10; // No Error
  //~^ ERROR: Invalid use of 'arguments' in strict mode.
}
var f2NoError = () => {
  var arguments = 10; // no error
  //~^ ERROR: Invalid use of 'arguments' in strict mode.
}

var d0 = ({ arguments }: any) => {}
//~^ ERROR: Invalid use of 'arguments' in strict mode.
var d1 = ({ a: arguments }: any) => {}
//~^ ERROR: Invalid use of 'arguments' in strict mode.
var d2 = ({ arguments: a }: any) => {}
var d3 = ([...arguments]: any) => {}
//~^ ERROR: Invalid use of 'arguments' in strict mode.
var d4 = ([...{arguments}]) => {}
//~^ ERROR: Invalid use of 'arguments' in strict mode.
var d5 = ([arguments]) => {}
//~^ ERROR: Invalid use of 'arguments' in strict mode.
