// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classPropInitializationInferenceWithElementAccess.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

export class Cls {
    x;
    y;
    z;
    
    0;

    constructor(seed: number) {
        this['x'] = [seed];
        this['y'] = { seed };
        this['z'] = `${seed}`;

        this[0] = [seed];
    }
}