function a0() {
  {
    var x = 1;
    () => (x);
  }
  {
    var x = 1;
  }
}
function a1() {
  {
    var x;
  }
  {
    var x = 1;
    () => (x);
  }
}
function a2() {
  {
    var x = 1;
    () => (x);
  }
  {
    var x;
    () => (x);
  }
}
function a3() {
  {
    var x = 1;
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
  {
    var x;
  }
  switch (1) {
    case 1:
      var x;
      () => (x);
      break;
    
  }
}
function a5() {
  {
    var x;
    () => (x);
  }
  switch (1) {
    case 1:
      var x;
      break;
    
  }
}
function a6() {
  switch (1) {
    case 1:
      var x;
      break;
    
  }
  switch (1) {
    case 1:
      var x;
      break;
    
  }
}
function a7() {
  switch (1) {
    case 1:
      var x;
      () => (x);
      break;
    
  }
  switch (1) {
    case 1:
      var x;
      break;
    
  }
}
function a8() {
  switch (1) {
    case 1:
      var x;
      break;
    
  }
  switch (1) {
    case 1:
      var x;
      () => (x);
      break;
    
  }
}
function a9() {
  switch (1) {
    case 1:
      var x;
      () => (x);
      break;
    
  }
  switch (1) {
    case 1:
      var x;
      () => (x);
      break;
    
  }
}