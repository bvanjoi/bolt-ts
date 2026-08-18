// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/optionalAccessorsInInterface1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
defineMyProperty({}, 'name', {
  get: function () {
    return 5;
  }  
});
defineMyProperty2({}, 'name', {
  get: function () {
    return 5;
  }  
});