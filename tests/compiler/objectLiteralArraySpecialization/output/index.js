var thing = create([{
  name: 'bob',
  id: 24  
}, {
  name: 'doug',
  id: 32  
}]);
thing.doSomething((x, y) => (x.name === 'bob'));