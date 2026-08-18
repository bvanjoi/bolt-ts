// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/multipleInferenceContexts.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict

var r2 = Moon({
  data: {
      msg: ''    
  },
  render() {
    var h = (x) => (x);
    return h(this.get('msg'));
  }  
});