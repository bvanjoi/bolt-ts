// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/circularlySimplifyingConditionalTypesNoCrash.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
// Circularly self constraining type, defered thanks to mapping
// Inference target is also mapped _and_ optional
// Then intersected with and indexed via Omit and &
// Then strictly compared with another signature in its context

var myStoreConnect = function (mapStateToProps, mapDispatchToProps, mergeProps, options = {}) {
  return connect(mapStateToProps, mapDispatchToProps, mergeProps, options);
};
export {  }