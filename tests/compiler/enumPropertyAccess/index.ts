// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/enumPropertyAccess.ts`, Apache-2.0 License

enum Colors {
    Red,
    Green
}

var x = Colors.Red; // type of 'x' should be 'Colors'
var p = x.Green; // error
//~^ ERROR: Property 'Green' does not exist on type 'Colors.Red'.
x.toFixed(); // ok

// Now with generics
function fill<B extends Colors>(f: B) {
    f.Green; // error
    //~^ ERROR: Property 'Green' does not exist on type 'B'.
    f.toFixed(); // ok
}