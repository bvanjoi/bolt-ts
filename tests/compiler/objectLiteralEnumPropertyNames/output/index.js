var Strs = {};
(function (Strs) {

  Strs[Strs['A'] = 'a'] = 'A'
  Strs[Strs['B'] = 'b'] = 'B'
})(Strs);
var x = {
  [Strs.A]: 'xo',
  [Strs.B]: 'xe'  
};
var ux = {
  [Strs.A]: 'xo',
  [Strs.B]: 'xe'  
};
var y = {
  ['a']: 'yo',
  ['b']: 'ye'  
};
var a = 'a';
var b = 'b';
var z = {
  [a]: 'zo',
  [b]: 'ze'  
};
var uz = {
  [a]: 'zo',
  [b]: 'ze'  
};
var Nums = {};
(function (Nums) {

  Nums[Nums['A'] = 0] = 'A'
  Nums[Nums['B'] = 0] = 'B'
})(Nums);
var n = {
  [Nums.A]: 1,
  [Nums.B]: 2  
};
var un = {
  [Nums.A]: 3,
  [Nums.B]: 4  
};
var an = 0;
var bn = 1;
var m = {
  [an]: 5,
  [bn]: 6  
};
var um = {
  [an]: 7,
  [bn]: 8  
};