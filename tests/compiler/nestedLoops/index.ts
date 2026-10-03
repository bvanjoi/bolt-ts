// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nestedLoops.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

export class Test  {
    constructor() {

        let outerArray: Array<number> = [1, 2, 3];
        let innerArray: Array<number> = [1, 2, 3];

        for (let outer of outerArray)
            for (let inner of innerArray) {
                this.aFunction((newValue, oldValue) => {
                    let x = outer + inner + newValue;
                });
            }
    }

    public aFunction(func: (newValue: any, oldValue: any) => void): void {
    }
}
