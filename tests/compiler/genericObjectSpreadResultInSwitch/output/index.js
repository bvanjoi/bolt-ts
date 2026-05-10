var getType = (params) => {
  var {foo, rest} = params;
  return rest
};

switch (params.tag) {
  case 'a':
    {
      var result = getType(params).type;
      break;
    }
  
  case 'b':
    {
      var result = getType(params).type;
      break;
    }
  
}