// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/truthinessCallExpressionCoercion.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

function onlyErrorsWhenTestingNonNullableFunctionType(required: () => boolean, optional?: () => boolean) {
    if (required) { // error
        //~^ ERROR: This condition will always return true since this function is always defined. Did you mean to call it instead?
    }

    if (optional) { // ok
    }

    if (!!required) { // ok
    }

    if (required()) { // ok
    }
}

function onlyErrorsWhenUnusedInBody() {
    function test() { return Math.random() > 0.5; }

    if (test) { // error
        //~^ ERROR: This condition will always return true since this function is always defined. Did you mean to call it instead?
        console.log('test');
    }
    
    if (test) { // ok
        console.log(test);
    }

    if (test) { // ok
        test();
    }
    
    if (test) { // ok
        [() => null].forEach(() => {
            test();
        });
    }
    
    if (test) { // error
        //~^ ERROR: This condition will always return true since this function is always defined. Did you mean to call it instead?
        [() => null].forEach(test => {
            test();
        });
    }
}

function checksPropertyAccess() {
    const x = {
        foo: {
            bar() { return true; }
        }
    }

    if (x.foo.bar) { // error
        //~^ ERROR: This condition will always return true since this function is always defined. Did you mean to call it instead?
    }

    if (x.foo.bar) { // ok
        x.foo.bar;
    }
}

class Foo {
    maybeIsUser?: () => boolean;

    isUser() {
        return true;
    }

    test() {
        if (this.isUser) { // error
        //~^ ERROR: This condition will always return true since this function is always defined. Did you mean to call it instead?
        }

        if (this.maybeIsUser) { // ok
        }
    }
}

// Test for GH-35557 where ids were not assigned for a symbol.
function A(stats: StatsBase<any>) {
    if (stats.isDirectory) { // err
        //~^ ERROR: This condition will always return true since this function is always defined. Did you mean to call it instead?
        console.log(`[Directory] ${stats.ctime}`)
    }
}

function B(a: Nested, b: Nested) {
    if (a.stats.isDirectory) { // err
        //~^ ERROR: This condition will always return true since this function is always defined. Did you mean to call it instead?
        b.stats.isDirectory(); 
    }
    if (a.stats.isDirectory) { // ok
        a.stats.isDirectory();
    }
} 

interface StatsBase<T> {
    isDirectory(): boolean;
    ctime: number;
}

interface Nested {
    stats: StatsBase<any>;
}
