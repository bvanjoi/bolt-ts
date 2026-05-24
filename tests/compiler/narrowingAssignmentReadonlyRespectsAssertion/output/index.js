function subDataFunc() {
  return [{
      val1: 'a',
    val2: ['a', 'b', 'c']    
  }, {
      val1: 2,
    val2: [1, 2, 3]    
  }, {
      val1: ['a', 'z'],
    val2: ['x', 'y', 'z']    
  }, {
      val1: [5, 10],
    val2: [10, 100, 1000]    
  }]
}
function dataFunc(subFunc) {
  return {
      cases: subFunc()    
  }
}
function testFunc() {
  var fixture = dataFunc(subDataFunc);
  fixture.cases.forEach(({val1, val2}) => {
    if (Array.isArray(val1)) {
      var reversedVal1 = val1.slice().reverse();
      console.log(reversedVal1);
    } else {
      console.log(val1);
    }
    
    console.log(val2);
  });
}
testFunc();