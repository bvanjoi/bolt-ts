function f(...args) {
  console.log(args);
}
f`'💩'${' should be converted to '}'\uD83D\uDCA9'`;