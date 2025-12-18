for ( var x = 1, y = 2; x < y; ++x , --y) {
  var a = () => (x++ + y++);
  if (x == 1) {
    break;
  } else {
    y = 5;
  }
  
}
for ( var x = 1, y = 2; x < y; ++x , --y) {
  var a = () => (x++ + y++);
  if (x == 1) {
    continue;
  } else {
    y = 5;
  }
  
}
loop: for ( var x = 1, y = 2; x < y; ++x , --y) {
  var a = () => (x++ + y++);
  if (x == 1) {
    break loop;
  } else {
    y = 5;
  }
  
}
loop: for ( var x = 1, y = 2; x < y; ++x , --y) {
  var a = () => (x++ + y++);
  if (x == 1) {
    continue loop;
  } else {
    y = 5;
  }
  
}