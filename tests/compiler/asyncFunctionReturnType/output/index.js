async function fAsync() {
  return [1, true]
}
async function fAsyncExplicit() {
  return [1, true]
}
async function fIndexedTypeForStringProp(obj) {
  return obj.stringProp
}
async function fIndexedTypeForPromiseOfStringProp(obj) {
  return Promise.resolve(obj.stringProp)
}
async function fIndexedTypeForExplicitPromiseOfStringProp(obj) {
  return Promise.resolve(obj.stringProp)
}
async function fIndexedTypeForAnyProp(obj) {
  return obj.anyProp
}
async function fIndexedTypeForPromiseOfAnyProp(obj) {
  return Promise.resolve(obj.anyProp)
}
async function fIndexedTypeForExplicitPromiseOfAnyProp(obj) {
  return Promise.resolve(obj.anyProp)
}
async function fGenericIndexedTypeForStringProp(obj) {
  return obj.stringProp
}
async function fGenericIndexedTypeForPromiseOfStringProp(obj) {
  return Promise.resolve(obj.stringProp)
}
async function fGenericIndexedTypeForExplicitPromiseOfStringProp(obj) {
  return Promise.resolve(obj.stringProp)
}
async function fGenericIndexedTypeForAnyProp(obj) {
  return obj.anyProp
}
async function fGenericIndexedTypeForPromiseOfAnyProp(obj) {
  return Promise.resolve(obj.anyProp)
}
async function fGenericIndexedTypeForExplicitPromiseOfAnyProp(obj) {
  return Promise.resolve(obj.anyProp)
}
async function fGenericIndexedTypeForKProp(obj, key) {
  return obj[key]
}
async function fGenericIndexedTypeForPromiseOfKProp(obj, key) {
  return Promise.resolve(obj[key])
}
async function fGenericIndexedTypeForExplicitPromiseOfKProp(obj, key) {
  return Promise.resolve(obj[key])
}