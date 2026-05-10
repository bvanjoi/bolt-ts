function foo(obj) {
  return unboxify(obj)
}
var qq = foo({
  u: {
      value: 10    
  },
  v: {
      value: 'hello'    
  }  
});
var left = {};
var right = {};
var ok = (at) => (({
  lr: at.lr(at.str, at.num)  
}));
var orphaned = (at) => (at.dict(ok(at)));
var leftOk = ok(left);
var leftOrphaned = orphaned(left);
var rightOk = ok(right);
var rightOrphaned = orphaned(right);