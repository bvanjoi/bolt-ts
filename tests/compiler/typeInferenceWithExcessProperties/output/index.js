function parrot(obj) {
  return obj
}
parrot({
  name: 'TypeScript'  
});
parrot({
  name: 'TypeScript',
  age: 5  
});
parrot({
  name: 'TypeScript',
  age: function () {}  
});
parrot({
  name: 'TypeScript',
  sayHello() {}  
});