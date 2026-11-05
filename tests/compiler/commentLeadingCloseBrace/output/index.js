// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentLeadingCloseBrace.ts`, Apache-2.0 License
//@compiler-options: strict=false
//@compiler-options: target=es2015
function ifelse() {
  if (commentedParameters(1, 2)) {
    /*comment1*/
    commentedParameters(3, 4);
  /*comment2*/
  } else {
    commentedParameters(5, 6);
  }
  
}