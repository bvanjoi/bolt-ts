// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/truthinessCallExpressionCoercion1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

function onlyErrorsWhenTestingNonNullableFunctionType(required: () => boolean, optional?: () => boolean) {
    // error
    required ? console.log('required') : undefined;
    //~^ ERROR: This condition will always return true since this function is always defined.

    // ok
    optional ? console.log('optional') : undefined;

    // ok
    !!required ? console.log('not required') : undefined;

    // ok
    required() ? console.log('required call') : undefined;
}

function onlyErrorsWhenUnusedInBody() {
    function test() { return Math.random() > 0.5; }

    // error
    test ? console.log('test') : undefined;
    //~^ ERROR: This condition will always return true since this function is always defined.

    // ok
    test ? console.log(test) : undefined;

    // ok
    test ? test() : undefined;

    // ok
    test
        ? [() => null].forEach(() => { test(); })
        : undefined;

    // error
    test
    //~^ ERROR: This condition will always return true since this function is always defined.
        ? [() => null].forEach(test => { test() })
        : undefined;
}

function checksPropertyAccess() {
    const x = {
        foo: {
            bar() { return true; }
        }
    }

    // error
    x.foo.bar ? console.log('x.foo.bar') : undefined;
    //~^ ERROR: This condition will always return true since this function is always defined.

    // ok
    x.foo.bar ? x.foo.bar : undefined;

    var chrome = {
        platformKeys: {
            subtleCrypto() {
                return {
                    sign() {},
                    exportKey() { return true }
                }
            }
        }
    }
    // ok
    if (chrome.platformKeys.subtleCrypto().exportKey) {
        chrome.platformKeys.subtleCrypto().exportKey
    }
}

class Foo {
    maybeIsUser?: () => boolean;

    isUser() {
        return true;
    }

    test() {
        // error
        this.isUser ? console.log('this.isUser') : undefined;
    //~^ ERROR: This condition will always return true since this function is always defined.

        // ok
        this.maybeIsUser ? console.log('this.maybeIsUser') : undefined;

        // ok
        if (this.isUser) {
            this.isUser();
        }
    }
}
