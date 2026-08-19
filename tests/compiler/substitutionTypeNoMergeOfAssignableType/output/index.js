function makeEntityStore(config) {
  return {};
}
var myTest = makeEntityStore({
  test: {
      fields: {
          id: {}      
    }    
  }  
});
myTest.test;