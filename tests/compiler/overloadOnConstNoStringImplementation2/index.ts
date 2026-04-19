// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/overloadOnConstNoStringImplementation2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface I {
    x1(a: number, callback: (x: 'hi') => number);
}

class C implements I {
    x1(a: number, callback: (x: 'hi') => number);
    x1(a: number, callback: (x: any) => number) {
        callback('hi');
        callback('bye');
        var hm = "hm";
        callback(hm);
        callback(1);
    }
}

declare var c: C;
c.x1(1, (x: 'hi') => { return 1; } );
c.x1(1, (x: 'bye') => { return 1; } ); 
//~^ ERROR: Argument of type '(x: "bye") => number' is not assignable to parameter of type '(x: "hi") => number'.
c.x1(1, (x: string) => { return 1; } );
c.x1(1, (x: number) => { return 1; } );
//~^ ERROR: Argument of type '(x: number) => number' is not assignable to parameter of type '(x: "hi") => number'.