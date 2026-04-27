test({
  props: {
      foo: ''    
  },
  data() {
    return {
          bar: true      
    }
  },
  watch: {
      foo(newVal, oldVal) {
      this.bar = false;
    }    
  }  
});