function getSecurity(level) {
  switch (level) {
    case 0:
    case 1:
    case 2:
      return 'Hi'
    
    case 3:
    case 4:
      return 'hello'
    
    case 5:
    default:
      return 'world'
    
  }
}