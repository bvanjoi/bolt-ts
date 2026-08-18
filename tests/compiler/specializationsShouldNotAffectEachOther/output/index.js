// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/specializationsShouldNotAffectEachOther.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
var series;
function foo() {
  var seriesExtent = (series) => (null);
  var series2;
  series2.map(seriesExtent);
  return null;
}
var keyExtent2 = series.data.map(function (d) {
  return d;
});