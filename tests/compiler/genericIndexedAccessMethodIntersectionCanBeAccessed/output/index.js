// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericIndexedAccessMethodIntersectionCanBeAccessed.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var createService = (ServiceCtr) => {
  Object.keys(ServiceCtr).forEach((key) => {
    var method = (ServiceCtr)[key];
    var {__$daemonMode, __$action, id} = method;
  });
};