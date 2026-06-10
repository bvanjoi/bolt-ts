var x = 1;
export function foo(y) {
  if (y <= x++) return y <= x++
  
  if (y <= x--) return y <= x--
  
  if (y <= ++x) return y <= ++x
  
  if (y <= --x) return y <= --x
  
  x++;
  x--;
  ++x;
  --x;
}
export { x }