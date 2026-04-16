function a0() {
  for ( var x in []) {
    x = x + 1;
  }
  for ( var x; ; ) {
    x = x + 2;
  }
}
function a1() {
  for ( var x in []) {
    x = x + 1;
    () => (x);
  }
  for ( var x; ; ) {
    x = x + 2;
  }
}
function a2() {
  for ( var x in []) {
    x = x + 1;
  }
  for ( var x; ; ) {
    x = x + 2;
    () => (x);
  }
}
function a3() {
  for ( var x in []) {
    x = x + 1;
    () => (x);
  }
  for ( var x; false; ) {
    x = x + 2;
    () => (x);
  }
  switch (1) {
    case 1:
      var x;
      () => (x);
      break;
    
  }
}
function a4() {
  for ( var x in []) {
    x = x + 1;
  }
  for ( var x; false; ) {
    x = x + 2;
  }
  switch (1) {
    case 1:
      var x;
      () => (x);
      break;
    
  }
}
function a5() {
  var y;
  for ( var x in []) {
    x = x + 1;
  }
  for ( var x; false; ) {
    x = x + 2;
    () => (x);
  }
  switch (1) {
    case 1:
      var x;
      break;
    
  }
}