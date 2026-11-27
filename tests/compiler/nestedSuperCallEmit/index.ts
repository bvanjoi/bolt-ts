// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nestedSuperCallEmit.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

// https://github.com/microsoft/TypeScript/issues/55646
abstract class Foo {
    constructor(shouldThrow: boolean) {
        if (shouldThrow) {
            throw new Error('Please retry');
        } else {
            console.log('OK');
        }
    }
}

class Bar extends Foo {
    constructor() {
        try {
            super(true);
        } catch (e: unknown) {
            console.log('Error: ' + (e as Error).message);
            // retry
            super(false);
        }
    }
}

new Bar();
