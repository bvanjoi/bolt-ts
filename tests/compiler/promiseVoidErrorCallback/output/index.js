function f1() {
  return Promise.resolve({
      __t1: 'foo_t1'    
  });
}
function f2(x) {
  return {
      __t2: x.__t1 + ':foo_21'    
  };
}
var x3 = f1().then(f2, (e) => {
  throw e
}).then((x) => ({
  __t3: x.__t2 + 'bar'  
}));