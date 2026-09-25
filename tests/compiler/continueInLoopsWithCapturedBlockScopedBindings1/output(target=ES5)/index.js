function foo() {
  for ( var i of [0, 1]) {
    if (i === 0) {
      continue;
    }
    
    (() => (i))();
  }
}