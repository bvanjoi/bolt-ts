var fn1 = () => (({
  test: (value) => (value),
  extraValue: () => {}  
}));
var fn1async = () => (({
  test: async (value) => (value),
  extraValue: () => {}  
}));
var fn2 = () => (({
  test: (value) => (value),
  extraValue: () => {}  
}));
var fn2async = () => (({
  test: async (value) => (value),
  extraValue: () => {}  
}));
var fn3 = () => (({
  extraValue: () => {},
  test: (value) => (value)  
}));
var fn3async = () => (({
  extraValue: () => {},
  test: async (value) => (value)  
}));
var fn4 = () => (({
  extraValue: '',
  test: (value) => (value)  
}));
var fn4async = () => (({
  extraValue: '',
  test: async (value) => (value)  
}));