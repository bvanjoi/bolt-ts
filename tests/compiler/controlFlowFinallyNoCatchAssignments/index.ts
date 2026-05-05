// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/controlFlowFinallyNoCatchAssignments.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

let x: number;
x = Math.random();
let a: number;
try {
    if (x) {
        a = 1;
    } else {
        a = 2;
    }
} finally {
    console.log(x);
}

console.log(a); // <- error here