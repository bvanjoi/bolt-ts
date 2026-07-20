// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/restParameterNoTypeAnnotation.ts`, Apache-2.0 License

function foo(...rest) {   
//~^ ERROR: Rest parameter 'rest' implicitly has an 'any[]' type.
  var x: number = rest[0];
  var y: string = rest[0];
  return x;   
}  
