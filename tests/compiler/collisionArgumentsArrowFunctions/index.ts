// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/collisionArgumentsArrowFunctions.ts`, Apache-2.0 License

var f1 = (i: number, ...arguments) => { //arguments is error
//~^ ERROR: Duplicate identifier 'arguments'. Compiler uses 'arguments' to initialize rest parameters.
  var arguments: any[]; // no error
}
var f12 = (arguments: number, ...rest) => { //arguments is error
//~^ ERROR: Duplicate identifier 'arguments'. Compiler uses 'arguments' to initialize rest parameters.
  var arguments = 10; // no error
}
var f1NoError = (arguments: number) => { // no error
  var arguments = 10; // no error
}

var f2 = (...restParameters) => {
  var arguments = 10; // No Error
}
var f2NoError = () => {
  var arguments = 10; // no error
}