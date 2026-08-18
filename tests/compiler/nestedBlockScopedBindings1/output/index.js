// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/nestedBlockScopedBindings1.ts`, Apache-2.0 License
function a0() {
  {
    var x = 1;
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
  }
}
function a2() {
  {
    var x = 1;
  }
  {
    var x;
  }
}
function a3() {
  {
    var x = 1;
  }
  switch (1) {
    case 1:
      var x;
      break;
    
  }
}
function a4() {
  {
    var x;
  }
  switch (1) {
    case 1:
      var x = 1;
      break;
    
  }
}