var narrowToLiterals = (str) => {
  switch (str) {
    case 'abc':
      {
        return str;
      }
    
    default:
      return 'defaultValue';
    
  }
};
var narrowToString = (str, someOtherStr) => {
  switch (str) {
    case 'abc':
      {
        return str;
      }
    
    case someOtherStr:
      {
        return str;
      }
    
    default:
      return 'defaultValue';
    
  }
};
var narrowToStringOrNumber = (str, someNumber) => {
  switch (str) {
    case 'abc':
      {
        return str;
      }
    
    case someNumber:
      {
        return str;
      }
    
    default:
      return 'defaultValue';
    
  }
};