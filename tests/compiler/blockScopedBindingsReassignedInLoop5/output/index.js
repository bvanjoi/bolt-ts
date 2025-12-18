for ( var x = 1, y = 2; x < y; ++x , --y) {
  var a = () => (x++ + y++);
  if (x == 1) break; else y = 5;
  
}