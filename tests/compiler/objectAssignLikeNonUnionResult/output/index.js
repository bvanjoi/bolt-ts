var defaultValue = {
  field: 1  
};
var data1 = assign(defaultValue, Date.now() > 3 ? {
  field: 2  
} : {});