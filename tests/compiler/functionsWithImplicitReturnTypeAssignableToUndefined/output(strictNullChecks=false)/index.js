// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionsWithImplicitReturnTypeAssignableToUndefined.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: noImplicitReturns=false
//@[strictNullChecks=true]   compiler-options: strictNullChecks
//@[strictNullChecks=false]  compiler-options: strictNullChecks=false
function f1() {
  if (Math.random() < 0.5) return true;
  
// Implicit return, but undefined is always assignable to unknown.
}
function f2() {
  if (Math.random() < 0.5) return true;
  
// Implicit return, but undefined is always assignable to unknown.
}
function f3() {// Implicit return, but undefined is always assignable to any.
}
function f4() {// Implicit return, but undefined is always assignable to void.
}
function f5() {
  //~[strictNullChecks=true]^ ERROR: Function lacks ending return statement and return type does not include 'undefined'.
  if (Math.random() < 0.5) return {};
  
// Implicit return, but undefined is assignable to object when strictNullChecks is off.
}
function f6() {
  //~[strictNullChecks=true]^ ERROR: Function lacks ending return statement and return type does not include 'undefined'.
  if (Math.random() < 0.5) return {
      'foo': true    
  };
  
// Implicit return, but undefined is assignable to records (which are just fancy objects)
// when strictNullChecks is off.
}
function f7() {
  //~[strictNullChecks=true]^ ERROR: Function lacks ending return statement and return type does not include 'undefined'.
  if (Math.random() < 0.5) return null;
  
// Implicit return, but undefined is assignable to null when strictNullChecks is off.
}
function f8() {
  //~[strictNullChecks=true]^ ERROR: Function lacks ending return statement and return type does not include 'undefined'.
  if (Math.random() < 0.5) return 'foo';
  
// Implicit return, but undefined is assignable to null when strictNullChecks is off.
}