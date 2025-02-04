function fib(x) {
  return x <= 1 ? x : fib(x - 1) + fib(x - 2)
}
var result = fib(5)
var result1 = fib(5)
function fib2(x) {
  return x <= 1 ? x : fib2(x - 1) + fib2(x - 2)
}
var result2 = fib2(5)
var result21 = fib2(5)