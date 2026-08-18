// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/flowAfterFinally1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
var result;
openFile();
try {
  result = someOperation();
}finally {
  closeFile();
}
result;