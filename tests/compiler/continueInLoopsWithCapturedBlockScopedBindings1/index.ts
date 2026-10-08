// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/continueInLoopsWithCapturedBlockScopedBindings1.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

function foo() {
    for (const i of [0, 1]) {
        if (i === 0) {
            continue;
        }

        // Trigger non-simple-loop emit
        (() => {
            return i;
        })();
    }
}