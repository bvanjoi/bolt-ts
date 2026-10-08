// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/letConstMatchingParameterNames.ts`, Apache-2.0 License
//@compiler-options: strict=false
//@compiler-options: lib=[es5]
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
var parent = true;
var parent2 = true;
function a() {
  var parent = 1;
  var parent2 = 2;
  function b(parent, parent2) {
    use(parent);
    use(parent2);
  }
}