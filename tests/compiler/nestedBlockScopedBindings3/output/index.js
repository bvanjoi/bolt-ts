function a0() {
  {
    for ( var x = 0; x < 1; ) {
      () => (x);
    }
  }
  {
    for ( var x; ; ) {
      () => (x);
    }
  }
}
function a1() {
  for ( var x; x < 1; ) {
    () => (x);
  }
  for ( var x; ; ) {
    () => (x);
  }
}
function a2() {
  for ( var x; x < 1; ) {
    x = x + 1;
  }
  for ( var x; ; ) {
    x = x + 2;
  }
}
function a3() {
  for ( var x; x < 1; ) {
    x = x + 1;
  }
  switch (1) {
    case 1:
      var x;
      break;
    
  }
}
function a4() {
  for ( var x; x < 1; ) {
    x = x + 1;
    () => (x);
  }
  switch (1) {
    case 1:
      var x;
      break;
    
  }
}
function a5() {
  for ( var x; x < 1; ) {
    x = x + 1;
    () => (x);
  }
  switch (1) {
    case 1:
      var x;
      () => (x);
      break;
    
  }
}