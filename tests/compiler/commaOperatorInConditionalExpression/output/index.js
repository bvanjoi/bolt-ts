function f(m) {
  [1, 2, 3].map((i) => (true ? {
      [m]: i    
  } : {
      [m]: i + 1    
  }));
  var a0 = [1, 2, 3].map((i) => (true ? {
      [m]: i    
  } : {
      [m]: i + 1    
  }));
  var a1 = {
      [m]: 42    
  };
}