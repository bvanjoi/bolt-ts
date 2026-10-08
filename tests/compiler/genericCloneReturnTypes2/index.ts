// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericCloneReturnTypes2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class MyList<T> {
    public size: number;
    public data: T[];
    constructor(n: number) {
        this.size = n;
        this.data = new Array<T>(this.size);
    }
    public clone() {
        return new MyList<T>(this.size);
    }
}
var a: MyList<string>;
var b: MyList<any> = a.clone(); // ok
//~^ ERROR: Variable 'a' is used before being assigned.
var c: MyList<string> = a.clone(); // bug was there was an error on this line
//~^ ERROR: Variable 'a' is used before being assigned.
var d: MyList<number> = a.clone(); // error
//~^ ERROR: Variable 'a' is used before being assigned.
//~| ERROR: Type 'MyList<string>' is not assignable to type 'MyList<number>'.
