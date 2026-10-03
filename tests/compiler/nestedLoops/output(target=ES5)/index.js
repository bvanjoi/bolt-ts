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