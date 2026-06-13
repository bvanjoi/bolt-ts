// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/switchCaseCircularRefeference.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function f(x: {a: "A", b} | {a: "C", e}) {
    switch (x.a) {
    case x:
      //~^ ERROR: Type '{ a: "A"; b: any; } | { a: "C"; e: any; }' is not comparable to type '"A" | "C"'.
        break;
    }
}