// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/switchCaseNarrowsMatchingClausesEvenWhenNonMatchingClausesExist.ts`, Apache-2.0 License
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