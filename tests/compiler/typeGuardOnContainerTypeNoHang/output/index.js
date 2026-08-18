// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeGuardOnContainerTypeNoHang.ts`, Apache-2.0 License
var TypeGuards = {};
(function (TypeGuards) {

  function IsObject(value) {
    return typeof (value) === 'object';
  }
  TypeGuards.IsObject = IsObject;
  
})(TypeGuards);