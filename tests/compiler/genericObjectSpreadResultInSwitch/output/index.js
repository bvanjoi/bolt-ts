// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericObjectSpreadResultInSwitch.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var getType = (params) => {
  var {foo, ...rest} = params;
  return rest;
};

switch (params.tag) {
  case 'a':
    {
      var result = getType(params).type;
      break;
    }
  
  case 'b':
    {
      var result = getType(params).type;
      break;
    }
  
}