
var r2 = Moon({
  data: {
      msg: ''    
  },
  render() {
    var h = (x) => (x);
    return h(this.get('msg'));
  }  
});