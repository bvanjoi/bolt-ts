var obj = {};
var a = {
  ...obj,
  prop() {
    return {
          ...obj,
      metadata: 213      
    }
  }  
};