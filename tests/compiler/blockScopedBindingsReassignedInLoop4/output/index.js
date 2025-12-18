function f1() {
  for ( var x = 1, y = 2; x < y; ++x , --y) {
    var a = () => (x++ + y++);
    if (x == 1) {
      return 1
    } else {
      y = 5;
    }
    
  }
}