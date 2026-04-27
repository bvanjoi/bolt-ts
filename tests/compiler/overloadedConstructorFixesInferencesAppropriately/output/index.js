class AsyncLoader {
  constructor(...args) {}
}
function load() {
  return null
}
new AsyncLoader({
  asyncLoad: load,
  children: (result) => (result.success)  
});