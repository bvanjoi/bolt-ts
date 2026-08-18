// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/nestedBlockScopedBindings16.ts`, Apache-2.0 License
var x;
for ( ; false; ) {
  {
    var x;
    () => (x);
  }
}
var y;
for ( ; false; ) {
  {
    var y;
    y = 1;
  }
}
var z0;
for ( ; false; ) {
  switch (1) {
    case 1:
      var z0;
      () => (z0);
      break;
    
  }
}
var z;
for ( ; false; ) {
  switch (1) {
    case 1:
      var z;
      z = 1;
      break;
    
  }
}