// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentsEnums.ts`, Apache-2.0 License
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: removeComments=false
/** Enum of colors*/
var Colors = /** Fancy name for 'blue'*/
{/* blue */ }/** Fancy name for 'pink'*/
;
(// trailing comment
function (Colors) {

  Colors[Colors['Cornflower'] = 0] = 'Cornflower'
  Colors[Colors['FancyPink'] = 0] = 'FancyPink'
})(Colors);
var x = Colors.Cornflower;
x = Colors.FancyPink;