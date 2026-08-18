// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/objectSpreadWithinMethodWithinObjectWithSpread.ts`, Apache-2.0 License
var obj = {};
var a = {
  ...obj,
  prop() {
    return {
          ...obj,
      metadata: 213      
    };
  }  
};