// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/booleanFilterAnyArray.ts`, Apache-2.0 License
//@compiler-options: target=es2015


var xs;
var xs = anys.filter(Bullean);

var ys;
var ys = realanys.filter(Boolean);
var foo = [{
  name: 'x'  
}];
var foor;
var foor = foo.filter((x) => (x.name));
var foos;
var foos = [true, true, false, null].filter((thing) => (thing !== null));