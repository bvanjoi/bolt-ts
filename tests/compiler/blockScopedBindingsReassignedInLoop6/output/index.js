function f1() {
  for ( var [x, y] = [1, 2]; x < y; ++x , --y) {
    var a = () => (x++ + y++);
    if (x == 1) break; else if (y == 2) y = 5; else return 
    
    
  }
}
function f2() {
  for ( var [{a: x, b: {c: y}}] = [{
      a: 1,
    b: {
          c: 2      
    }    
  }]; x < y; ++x , --y) {
    var a = () => (x++ + y++);
    if (x == 1) break; else if (y == 2) y = 5; else return 
    
    
  }
}