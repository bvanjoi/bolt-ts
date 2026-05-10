class ClassA {
  constructor(entity, settings) {
    this.settings = settings}
}
class ConcreteClass {
  theName = 'myClass';
}
var thisGetsTheFalseError = new ClassA(new ConcreteClass(), {
  values: (o) => ([{
      value: o.theName,
    func: (x) => ('asdfkjhgfdfghjkjhgfdfghjklkjhgfdfghjklkjhgfghj')    
  }])  
});
var thisIsOk = new ClassA(new ConcreteClass(), {
  values: (o) => ([{
      value: o.theName,
    func: (x) => ('asdfkjhgfdfghjkjhgfdfghjklkjhgfdfghjklkjhgfghj')    
  }])  
});