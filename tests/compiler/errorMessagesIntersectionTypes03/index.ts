// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/errorMessagesIntersectionTypes03.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface A {
    a;
}

interface B {
    b;
}

function f<T, U extends A, V extends U>(): void {
    let t: T;
    let u: U;
    let v: V;

    let a_and_b: A & B;
    let t_and_b: T & B;

    t = a_and_b;  //~ERROR: Type 'A & B' is not assignable to type 'T'.
    u = a_and_b;  //~ERROR: Type 'A & B' is not assignable to type 'U'.
    v = a_and_b;  //~ERROR: Type 'A & B' is not assignable to type 'V'.

    t = t_and_b;
    u = t_and_b;  //~ERROR: Type 'T & B' is not assignable to type 'U'.
    v = t_and_b;  //~ERROR: Type 'T & B' is not assignable to type 'V'.
}