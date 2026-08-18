// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionOverloadsOutOfOrder.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class d {
    private foo(n: number): string;
    private foo(ns: any) {
        return ns.toString();
    }
    private foo(s: string): string;
    //~^ ERROR: Function implementation is missing or not immediately following the declaration.
}

class e {
    private foo(ns: any) {
        return ns.toString();
    }
    private foo(s: string): string;
    private foo(n: number): string;
    //~^ ERROR: Function implementation is missing or not immediately following the declaration.
}