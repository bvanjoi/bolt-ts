// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyForIn.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

var x: {}[] = [[1, 2, 3], ["hello"]];

for (var i in x) {
    for (var j in x[i]) {

        //Should yield an implicit 'any' error
        var _j = x[i][j];
        //~^ ERROR: Element implicitly has an 'any' type because expression of type 'string' can't be used to index type '{ }'.
    }

    for (var k in x[0]) {
        var k1 = x[0];

        //Should yield an implicit 'any' error
        var k2 = k1[k];
        //~^ ERROR: Element implicitly has an 'any' type because expression of type 'string' can't be used to index type '{ }'.
    }
}

for (var a in x) {
    // Should yield an implicit 'any' error.
    var b;

    var c = a || b;
}

var idx = 0;
var m = [1, 2, 3, 4, 5];
// Should yield an implicit 'any' error.
var n = [[]] || [];
//~^ ERROR: This kind of expression is always truthy.

for (n[idx++] in m);
//~^ ERROR: The left-hand side of a 'for...in' statement must be of type 'string' or 'any'.