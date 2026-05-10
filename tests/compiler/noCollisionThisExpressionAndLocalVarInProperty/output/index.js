class class1 {
  prop1 = {
      doStuff: (callback) => (() => {
      var _this = 2;
      return callback(_this)
    })    
  };
}
class class2 {
  constructor() {var _this = 2;}
  prop1 = {
      doStuff: (callback) => (() => (callback(10)))    
  };
}