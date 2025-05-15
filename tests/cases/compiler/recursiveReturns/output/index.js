// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/recursiveReturns.ts`, Apache-2.0 License
function R1() {
  R1();
  return 
}
function R2() {
  R2();
}
function R3(n) {
  if (n == 0) {} else //return;
  {
    R3(n--);
  }
  
}