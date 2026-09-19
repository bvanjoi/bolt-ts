function* gen() {
  var obj = {
      foo: 1,
    bar: 2    
  };
  for ( var key in obj) {
    yield key;
    delete obj.bar;
  }
}