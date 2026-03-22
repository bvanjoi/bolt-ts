

var test = () => (({
  prop: !value,
  run: () => {
    if (!a.b()) {
      return 'special'
    }
    
    return 'default'
  }  
}));