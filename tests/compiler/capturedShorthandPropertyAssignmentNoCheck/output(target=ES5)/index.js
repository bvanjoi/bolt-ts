// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/capturedShorthandPropertyAssignmentNoCheck.ts`, Apache-2.0 License
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
var fns = [];
for ( var value of [1, 2, 3]) {
  fns.push(() => (({
      value    
  })));
}
var result = fns.map((fn) => (fn()));
console.log(result);