// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/restElementAssignable.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
{
  var {...props} = {};
  // Use to fail
  var t1 = props;
  // Working equivalent
  var t2 = {};
}
{
  var {...props} = {
      a: 1,
    b: false,
    c: 'str'    
  };
  // Use to fail
  var t1 = props;
  // Working equivalent
  var t2 = {
      a: 1,
    b: false,
    c: 'str'    
  };
  ;
}