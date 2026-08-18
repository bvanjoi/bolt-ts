// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/vueLikeDataAndPropsInference.ts`, Apache-2.0 License
//@compiler-options: target=esnext
test({
  props: {
      foo: ''    
  },
  data() {
    return {
          bar: true      
    };
  },
  watch: {
      foo(newVal, oldVal) {
      this.bar = false;
    }    
  }  
});