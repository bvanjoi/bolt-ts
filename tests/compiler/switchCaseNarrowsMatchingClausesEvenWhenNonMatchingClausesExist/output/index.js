// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/switchCaseNarrowsMatchingClausesEvenWhenNonMatchingClausesExist.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var narrowToLiterals = (str) => {
  switch (str) {
    case 'abc':
      {
        // inferred type as `abc`
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
        // inferred type should be `abc`
        return str;
      }
    
    case someOtherStr:
      {
        // `string`
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
        // inferred type should be `abc`
        return str;
      }
    
    case someNumber:
      {
        // inferred type should be `number`
        return str;
      }
    
    default:
      return 'defaultValue';
    
  }
};