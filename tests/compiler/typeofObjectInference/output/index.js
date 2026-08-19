var val = 1;
function decorateA(fn) {
  return () => (fn({
      value: val    
  }));
}
var a = decorateA(({value}) => (5));
function decorateB(fn) {
  return () => (fn(val));
}
var b = decorateB((value) => (5));
function decorateC(fn) {
  return () => (fn({
      value: val    
  }));
}
var c = decorateC(({value}) => (5));
function decorateD(fn) {
  return () => (fn({
      value: val    
  }));
}
var d = decorateD(({value}) => (5));