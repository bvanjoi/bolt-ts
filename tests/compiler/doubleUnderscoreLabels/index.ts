// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/doubleUnderscoreLabels.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function doThing() {
    __call: while (true) {
        aLabel: for (let i = 0; i < 10; i++) {
            if (i === 3) {
                break __call;
            }
            if (i === 5) {
                break aLabel;
            }
        }
    }
}
doThing();