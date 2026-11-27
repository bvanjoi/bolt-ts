// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfacedecl.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: declaration

interface a0 {
    (): string;
    (a, b, c?: string): number;
    
    new (): string;
    new (s: string);

    [n: number]: ()=>string;
    [s: string]: any;

    p1;
    p2: string;
    p3?;
    p4?: number;
    p5: (s: number) =>string;

    f1();
    f2? ();
    f3(a: string): number;
    f4? (s: number): string;
}


interface a1 {
    [n: number]: number;
}

interface a2 {
    [s: string]: number;
}

interface a {
}

interface b extends a {
}

interface c extends a, b {
}

interface d extends a {
}

class c1 implements a {
}
var instance2 = new c1();