function assertNever(x) {
  return x
}
function IfWithString(val) {
  if (val['kind'] === 'B') {
    return val.b
  } else {
    return val.a
  }
  
}
function SwitchWithString(val) {
  switch (val['kind']) {
    case 'A':
      return val.a
    
    case 'B':
      return val.b
    
    default:
      return assertNever(val)
    
  }
}
function IfWithTemplate(val) {
  if (val[`kind`] === 'B') {
    return val.b
  } else {
    return val.a
  }
  
}
function SwitchWithTemplate(val) {
  switch (val[`kind`]) {
    case 'A':
      return val.a
    
    case 'B':
      return val.b
    
    default:
      return assertNever(val)
    
  }
}