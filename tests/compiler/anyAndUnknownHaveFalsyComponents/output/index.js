
var y1 = x1 && 3;

function foo1() {
  return {
      display: 'block',
    ...(isTreeHeader1 && {
          display: 'flex'      
    })    
  };
}

var y2 = x2 && 3;

function foo2() {
  return {
      display: 'block',
    ...(isTreeHeader1 && {
          display: 'flex'      
    })    
  };
}