namespace D {
  //~^ ERROR: A namespace declaration cannot be in a different file from a class or function with which it is merged.
    export var y = "hi";
}
D.y;