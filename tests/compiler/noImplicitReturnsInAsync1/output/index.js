async function test(isError = false) {
  if (isError === true) {
    return ;
  }
  
  var x = await Promise.resolve('The test is passed without an error.');
}