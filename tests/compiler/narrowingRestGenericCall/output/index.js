// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowingRestGenericCall.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function call(obj, cb) {
  cb(obj);
}

call(obj, ({foo, ...rest}) => {
  console.log(rest.bar);
});