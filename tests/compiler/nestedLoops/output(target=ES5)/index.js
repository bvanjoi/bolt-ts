// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nestedLoops.ts`, Apache-2.0 License
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
export class Test {
  constructor() {var outerArray = [1, 2, 3];
    var innerArray = [1, 2, 3];
    for ( var outer of outerArray) for ( var inner of innerArray) {
      this.aFunction((newValue, oldValue) => {
        var x = outer + inner + newValue;
      });
    }}
  aFunction(func) {}
}