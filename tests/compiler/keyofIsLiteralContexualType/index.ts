// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/keyofIsLiteralContexualType.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// keyof T is a literal contextual type

function foo<T extends { a: string, b: string }>() {
    let a: (keyof T)[] = ["a", "b"];
    let b: (keyof T)[] = ["a", "b", "c"];
    //~^ ERROR: Type '"c"' is not assignable to type 'keyof T'.
}

// Repro from #12455

declare function pick<T, K extends keyof T>(obj: T, propNames: K[]): Pick<T, K>;

let x = pick({ a: 10, b: 20, c: 30 }, ["a", "c"]);
let b = x.b;  // Error
//~^ ERROR: Property 'b' does not exist on type 'Pick<{ a: number; b: number; c: number; }, "a" | "c">'.