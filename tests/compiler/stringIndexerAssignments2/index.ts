// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/stringIndexerAssignments2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C1 {
    [index: string]: string
    one!: string;
}

class C2 {
    one!: string;
}

class C3 {
    one!: number;
    two!: string;
}

declare var x: C1;
declare var a: C2;
declare var b: C3;

x = a;
//~^ ERROR: Type 'C2' is not assignable to type 'C1'.
x = b;
//~^ ERROR: Type 'C3' is not assignable to type 'C1'.
