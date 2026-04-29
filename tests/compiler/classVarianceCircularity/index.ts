// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classVarianceCircularity.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

function f() {
    const b = new Bar();
    // Uncomment to create error
    console.log(b.Value);
}

class Bar<T> {
    num!: number;
    // Or swap these two lines
    Field: number = (this as Bar<any>).num;
    Value = (this as Bar<any>).num;
}