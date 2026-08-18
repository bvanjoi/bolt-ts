// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/overloadedConstructorFixesInferencesAppropriately.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class AsyncLoader {
  constructor(...args) {}
}
function load() {
  return null;
}
new AsyncLoader({
  asyncLoad: load,
  children: (result) => (result.success)  
});