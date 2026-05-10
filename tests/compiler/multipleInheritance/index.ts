// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/multipleInheritance.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class B1 {
    public x;
}

class B2 {
    public x;
}

class C extends B1, B2 { // duplicate member
  //~^ ERROR: Syntax Error: Unexpected token ','
}

class D1 extends B1 {
}

class D2 extends B2 {
}

class E extends D1, D2 { // nope, duplicate member
  //~^ ERROR: Syntax Error: Unexpected token ','
}

class N {
    public y:number;
}

class ND extends N { // any is assignable to number
    public y;
}

class Good {
    public f:() => number = function() { return 0; }
    public g() { return 0; }
}

class Baad extends Good {
    public f(): number { return 0; }
    //~^ ERROR: Class 'Good' defines instance member property 'f', but extended class 'Baad' defines it as instance member function.
    public g(n:number) { return 0; }
    //~^ ERROR: Property 'g' in type 'Baad<Baad>' is not assignable to the same property in base type 'Good<Baad>'.
}
