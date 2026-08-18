// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unionOfClassCalls.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail

switch (tmp.get('t')) {
  case 'A':
    break;
  
  case 'B':
    break;
  
}
var arr = [];
var arr1 = [];
var arr2 = [];
arr.map((a, index) => (index));
arr.reduce((acc, a, index) => ([]), []);
arr.forEach((a, index) => (index));
arr1.map((a, index) => (index));
arr1.reduce((acc, a, index) => ([a]), []);
arr1.forEach((a, index) => (index));
arr2.map((a, index) => (index));
arr2.reduce((acc, a, index) => ([]), []);
arr2.forEach((a, index) => (index));

a.doThing().then((result) => {});