// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/duplicateClassElements.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class a {
    public a;
    public a; //~ERROR: Duplicate identifier 'a'.
    public b() {  //~ERROR: Duplicate function implementation.
    }
    public b() {  //~ERROR: Duplicate function implementation.
    }
    public x;
    get x() {
      //~^ ERROR: Duplicate identifier 'x'.
      //~| ERROR: Duplicate identifier 'x'.
        return 10;
    }
    set x(_x: number) {
      //~^ ERROR: Duplicate identifier 'x'.
      //~| ERROR: Duplicate identifier 'x'.
    }

    get y() {
        return "Hello";
    }
    set y(_y: string) {
    }

    public z() {
    }
    get z() { //~ERROR: Duplicate identifier 'z'.
        return "Hello";
    }
    set z(_y: string) { //~ERROR: Duplicate identifier 'z'.
    }

    get x2() {
        return 10;
    }
    set x2(_x: number) {
    }
    public x2;
    //~^ ERROR: Duplicate identifier 'x2'.
    //~| ERROR: Subsequent variable declarations must have the same type. Variable 'x2' must be of type 'number', but here has type 'any'.

    get z2() {
        return "Hello";
    }
    set z2(_y: string) {
    }
    public z2() { //~ERROR: Duplicate identifier 'z2'.
    }

}