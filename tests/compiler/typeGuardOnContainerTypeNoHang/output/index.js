var TypeGuards = {};
(function (TypeGuards) {

  function IsObject(value) {
    return typeof (value) === 'object'
  }
  TypeGuards.IsObject = IsObject;
  
})(TypeGuards);