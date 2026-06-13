var test1 = (async () => ([1, 'two']))();
var test2 = new Promise((resolve) => (resolve([1, 'two'])));
var obj = {
  foo: (() => ([1, 'two']))()  
};