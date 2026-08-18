// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/circularlySimplifyingConditionalTypesNoCrash.ts`, Apache-2.0 License
//@compiler-options: target=es2015

var myStoreConnect = function (mapStateToProps, mapDispatchToProps, mergeProps, options = {}) {
  return connect(mapStateToProps, mapDispatchToProps, mergeProps, options);
};
export {  }