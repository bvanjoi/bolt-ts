// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfacedeclWithIndexerErrors.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: strict=false

interface a0 {
    (): string;
    (a, b, c?: string): number;
    
    new (): string;
    new (s: string);

    [n: number]: ()=>string;
    [s: string]: ()=>string;

    p1;
    p2: string;
    //~^ ERROR: Property 'p2' of type 'string' is not assignable to 'string' index type '() => string'.
    p3?;
    p4?: number;
    //~^ ERROR: Property 'p4' of type 'number' is not assignable to 'string' index type '() => string'.
    p5: (s: number) =>string;
    //~^ ERROR: Property 'p5' of type '(s: number) => string' is not assignable to 'string' index type '() => string'.

    f1();
    f2? ();
    f3(a: string): number;
    //~^ ERROR: Property 'f3' of type '(a: string) => number' is not assignable to 'string' index type '() => string'.
    f4? (s: number): string;
    //~^ ERROR: Property 'f4' of type '(s: number) => string' is not assignable to 'string' index type '() => string'.
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

interface e extends number {
  //~^ ERROR: An interface cannot extend a primitive type like 'number'.
  //~| ERROR: An interface can only extend an object type or intersection of object types with statically known members.
}

interface f {
    prop: typeof string;
    //~^ ERROR: Cannot find name 'string'.
}

class c1 implements a {
}
var instance2 = new c1();
