



var a1 = foo(x1);
var a2 = foo(x2);
var a3 = foo(x3);
var a4 = foo(x4);
var param2 = Math.random() < 0.5 ? 'value2' : null;
var obj = {
  param1: 'value1',
  ...(param2 ? {
      param2    
  } : {})  
};
var query = Object.entries(obj).map(([k, v]) => (`${k}=${encodeURIComponent(v)}`)).join('&');