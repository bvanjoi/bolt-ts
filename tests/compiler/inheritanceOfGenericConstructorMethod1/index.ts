// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/inheritanceOfGenericConstructorMethod1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class A<T> { }
class B<T> extends A<T> {}
var a = new A<Date>();
var b1 = new B(); // no error
var b2: B<Date> = new B<Date>(); // no error
var b3 = new B<Date>(); // error, could not select overload for 'new' expression
