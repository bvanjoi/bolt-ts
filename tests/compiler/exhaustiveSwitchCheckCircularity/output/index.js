function f() {
  var foo = 'aaa';
  while (true) {
    switch (foo) {
      case 'aaa':
    }
    if (foo === 'aaa') {
      foo = 'bbb';
    } else if (isNever(foo)) {
      break;
    }
    
    
  }
}
function functionC() {
  var unionVal = 'A';
  while (true) {
    var key;
    switch (unionVal) {
      case 'A':
        {
          key = 'AA';
          break;
        }
      
    }
    functionB(key);
  }
}