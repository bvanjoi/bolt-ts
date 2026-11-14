export namespace Library {
    export type Bar = { a: number };
}
var x: Library.Bar; // should work
Library.foo; // should be an error
//~^ ERROR: Cannot find name 'Library'.