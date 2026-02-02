// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/contextualTypingArrayDestructuringWithDefaults.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

type I = { a: "a" };
let [ c0 = {a: "a"} ]: [I?] = [];
let [ x1, c1 = {a: "a"} ]: [number, I?] = [1];
let [ c_ = {a: "a"} ]: I[] = [];

// not a great example, expect an error
function foo() {
    let {length = {a: 1}}: [number] = [1];
    //~^ ERROR: Type '{ a: number; }' is not assignable to type '1'.
    return length;
}