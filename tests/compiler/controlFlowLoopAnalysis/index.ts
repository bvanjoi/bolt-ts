// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/controlFlowLoopAnalysis.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
//@compiler-options: noImplicitAny

let cond: boolean;

function foo(x: number): number { return 1; }

function test1() {
    let x: number | undefined;
    while (cond) {
        while (cond) {
            while (cond) {
                x = foo(x);
                //~^ ERROR: Argument of type 'undefined | number' is not assignable to parameter of type 'number'.
            }
        }
        x = 1;
    }
}

// Repro from #8418

function test2() {
    let x: number | undefined;
    x = 1;
    while (cond) {
        while (cond) {
            x = foo(x);
        }
    }
}

// Repro from #8511

function mapUntilCant<a, b>(
    values: a[],
    canTake: (value: a, index: number) => boolean,
    mapping: (value: a, index: number) => b
): b[] {
    let result: b[] = [];
    for (let index = 0, length = values.length; index < length; index++) {
        let value = values[index];
        if (canTake(value, index)) {
            result.push(mapping(value, index));
        } else {
            return result;
        }
    }
    return result;
}
