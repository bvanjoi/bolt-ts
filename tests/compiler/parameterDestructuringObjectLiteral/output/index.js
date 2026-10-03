var fn1 = (options) => {};
fn1({
  headers: {
      foo: 1    
  }  
});
var fn2 = ({headers = {}}) => {};
fn2({
  headers: {
      foo: 1    
  }  
});