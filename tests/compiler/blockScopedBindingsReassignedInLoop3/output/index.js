for ( var x = 1, y = 2; x < y; ++x , --y) {
  var a = () => (x++ + y++);
  if (x == 1) {
    break;
  } else {
    for ( var a = 1; a < 5; --a) {
      var f = () => (a);
      if (a) {
        a = x;
        break;
      } else {
        y++;
      }
      
    }
    y = 5;
  }
  
}
for ( var x = 1, y = 2; x < y; ++x , --y) {
  var a = () => (x++ + y++);
  if (x == 1) {
    continue;
  } else {
    for ( var a = 1; a < 5; --a) {
      var f = () => (a);
      if (a) {
        a = x;
        continue;
      } else {
        y++;
      }
      
    }
    y = 5;
  }
  
}
loop2: for ( var x = 1, y = 2; x < y; ++x , --y) {
  var a = () => (x++ + y++);
  if (x == 1) {
    break loop2;
  } else {
    loop1: for ( var a = 1; a < 5; --a) {
      var f = () => (a);
      if (a) {
        a = x;
        break loop1;
      } else {
        y++;
        break loop2;
      }
      
    }
    y = 5;
  }
  
}
loop2: for ( var x = 1, y = 2; x < y; ++x , --y) {
  var a = () => (x++ + y++);
  if (x == 1) {
    continue loop2;
  } else {
    loop1: for ( var a = 1; a < 5; --a) {
      var f = () => (a);
      if (a) {
        a = x;
        continue loop1;
      } else {
        y++;
        continue loop2;
      }
      
    }
    y = 5;
  }
  
}