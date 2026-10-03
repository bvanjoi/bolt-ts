var myFirstFunction = (param1) => {
  var newParam = Object.assign(param1, {
      otherProperty: 3    
  });
  mySecondFunction(newParam);
};
var mySecondFunction = (newParam) => (newParam);