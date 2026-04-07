// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/ambientConstLiterals.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

function f<T>(x: T): T {
    return x;
}

enum E { A, B, C, "non identifier" }

const c1 = "abc";
const c2 = 123;
const c3 = c1;
const c4 = c2;
const c5 = f(123);
const c6 = f(-123);
const c7 = true;
const c8 = E.A;
const c8b = E["non identifier"];
const c9 = { x: "abc" };
const c10 = [123];
const c11 = "abc" + "def";
const c12 = 123 + 456;
const c13 = Math.random() > 0.5 ? "abc" : "def";
const c14 = Math.random() > 0.5 ? 123 : 456;