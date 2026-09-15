var [a, b, c] = [1, 2, 3];
var [a1 = 10, b1 = 10, c1 = 10] = [1, 2, 3];
var [a2 = undefined, b2 = undefined, c2 = undefined] = [1, 2, 3];
var [a3 = undefined, b3 = null, c3 = undefined] = [1, 2, 3];
var [a4] = [undefined], [b4] = [null], c4 = undefined, d4 = null;
var {x, y, z} = {
  x: 1,
  y: 2,
  z: 3  
};
var {x1 = 10, y1 = 10, z1 = 10} = {
  x1: 1,
  y1: 2,
  z1: 3  
};
var {x2 = undefined, y2 = undefined, z2 = undefined} = {
  x2: 1,
  y2: 2,
  z2: 3  
};
var {x3 = undefined, y3 = null, z3 = undefined} = {
  x3: 1,
  y3: 2,
  z3: 3  
};
var {x4} = {
  x4: undefined  
}, {y4} = {
  y4: null  
};