// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noUnusedLocals_destructuringAssignment.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals

class C {
    private x = 0;

    m(): number {
        let x: number;
        ({ x } = this);
        return x;
    }

    private f(): Function {
      //~^ ERROR: 'f' is declared but its value is never read.
        let f: Function;
        ({ f } = this);
        return f;
    }
}
