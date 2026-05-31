// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parseShortform.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I {
    w: {
        z: I;
        (): boolean;
        [s: string]: { x: any; y: any; };
        [n: number]: { x: any; y: any; };
    };
    x: boolean;
    y: (s: string) => boolean;
    z: I; 
}