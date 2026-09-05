var removeF = ({f, ...rest}) => (rest);
var result = removeF({
  f: '',
  g: 3  
}).g;