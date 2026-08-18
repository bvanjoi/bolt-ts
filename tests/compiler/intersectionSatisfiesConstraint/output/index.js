// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/intersectionSatisfiesConstraint.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
var myFirstFunction = (param1) => {
  var newParam = Object.assign(param1, {
      otherProperty: 3    
  });
  mySecondFunction(newParam);
};
var mySecondFunction = (newParam) => (newParam);