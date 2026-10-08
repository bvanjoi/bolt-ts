// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classInConvertedLoopES5.ts`, Apache-2.0 License
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
var classesByRow = {};
for ( var row of ['1', '2', '3', '4', '5']) {
  class RowClass {
    row = row;
    static factory = () => (new RowClass());
  }
  classesByRow[row] = RowClass;
}