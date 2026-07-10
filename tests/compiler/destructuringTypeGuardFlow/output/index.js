var aFoo = {
  bar: 3,
  baz: 'b',
  nested: {
      a: 1,
    b: 'y'    
  }  
};
if (aFoo.bar && aFoo.nested.b) {
  var {bar, baz, nested: {a, b: text}} = aFoo;
  var right = aFoo.bar;
  var wrong = bar;
  var another = baz;
  var aAgain = a;
  var bAgain = text;
}

var bBar = {
  elem1: 7,
  elem2: aFoo  
};
if (bBar.elem2 && bBar.elem2.bar && bBar.elem2.nested.b) {
  var {bar, baz, nested: {a, b: text}} = bBar.elem2;
  var right = bBar.elem2.bar;
  var wrong = bar;
  var another = baz;
  var aAgain = a;
  var bAgain = text;
}
