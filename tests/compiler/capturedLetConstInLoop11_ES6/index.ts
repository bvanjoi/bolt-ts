// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/capturedLetConstInLoop11_ES6.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@compiler-options: strict=false

for (;;) {
    let x = 1;
    () => x;
}

function foo() {
    for (;;) {
        const a = 0;
        switch(a) {
            case 0: return () => a;
        }
    }
}

