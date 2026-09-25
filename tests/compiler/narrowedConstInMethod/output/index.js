function f() {
  var x = ({});
  if (x !== null) {
    return {
          bar() {
        return x.length;
      }      
    };
  }
  
}
function f2() {
  var x = ({});
  if (x !== null) {
    return class {
      bar() {
        return x.length;
      }
    };
  }
  
}