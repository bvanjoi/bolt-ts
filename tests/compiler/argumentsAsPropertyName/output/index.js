function myFunction(myType) {
  for ( var i = 0; i < 10; i++) {
    use(myType.arguments[i]);
    var x = 5;
    [1, 2, 3].forEach(function (j) {
      use(x);
    });
  }
}