// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/inheritanceOfGenericConstructorMethod2`, Apache-2.0 License

module M {
  export class C1 { }
  export class C2<T> { }
}
module N {
  export class D1 extends M.C1 { }
  export class D2<T> extends M.C2<T> { }
}

var c = new M.C2<number>(); // no error
var c0: M.C2<number> = new M.C2<number>(); // no error

var n = new N.D1(); // no error
var n0: N.D1 = new N.D1();

var n2 = new N.D2<number>(); // error
var n20: N.D2<number> = new N.D2<number>(); // error

var n3 = new N.D2(); // no error, D2<any>
var n30: N.D2<unknown> = new N.D2();
