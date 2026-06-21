function f(wrapped) {
  if (typeof wrapped.value !== 'string') {
    return '42'
  }
  
  return wrapped.value
}
function f1(obj) {
  if (typeof obj.kind === 'string') {
    obj;
  } else {
    obj;
  }
  
}
function f2(obj) {
  if (typeof obj.kind === 'string') {
    obj;
  } else {
    obj;
  }
  
}
function numberOk(wrapped) {
  if (typeof wrapped.value !== 'string') {
    return null
  }
  
  return wrapped.value
}
function booleanBad(wrapped) {
  if (typeof wrapped.value !== 'string') {
    return null
  }
  
  return wrapped.value
}
function booleanFixed(wrapped) {
  if (typeof (wrapped.value) !== 'string') {
    return null
  }
  
  return wrapped.value
}