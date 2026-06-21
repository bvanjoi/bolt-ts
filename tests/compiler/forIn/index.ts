// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/forIn.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var arr = null;
for (var i:number in arr) { // error
  //~^ ERROR: The left-hand side of a 'for...in' statement cannot use a type annotation.
    var x1 = arr[i];
    var y1 = arr[i];
}

for (var j in arr) { // ok
    var x2 = arr[j];
    var y2 = arr[j];
}

var arr2 = [];
for (j in arr2) { // ok
    var x3 = arr2[j];
    var y3 = arr2[j];
}

for (var l in arr) { 
   // error in the body
   k[l] = 1;
   //~^ ERROR: Cannot find name 'k'.
}