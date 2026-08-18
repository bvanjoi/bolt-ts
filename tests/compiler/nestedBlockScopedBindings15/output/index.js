// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/nestedBlockScopedBindings15.ts`, Apache-2.0 License
for ( ; false; ) {
  {
    var x;
    () => (x);
  }
}
for ( ; false; ) {
  {
    var y;
    y = 1;
  }
}
for ( ; false; ) {
  switch (1) {
    case 1:
      var z0;
      () => (z0);
      break;
    
  }
}
for ( ; false; ) {
  switch (1) {
    case 1:
      var z;
      z = 1;
      break;
    
  }
}