// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericObjectSpreadResultInSwitch.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@run-fail
var getType = (params) => {
  var {// Omit
  foo, ...rest} = params;
  return rest;
};

switch (params.tag) {
  case 'a':
    {
      var result = getType(params// TS 4.2: number
      // TS 4.3: string | number
      ).type;
      break;
    }
  
  case 'b':
    {
      var result = getType(params// TS 4.2: string
      // TS 4.3: string | number
      ).type;
      break;
    }
  
}