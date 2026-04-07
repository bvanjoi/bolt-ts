function example(parameters) {
  var {value = '123'} = parameters;
  return `${value}` === '345'
}
function example2(parameters) {
  var {value = '123'} = parameters;
  var b = `${value}`;
  return b
}