// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericTypeWithMultipleBases3.ts`, Apache-2.0 License

interface IA<T> {

  foo(x: T): T;
  
}
  
interface IB<T> {
  
  bar(x: T): T;
  
}
  
interface IC<T> extends IA<T>, IB<T> { }

var c: IC<number>;

var x = c.foo;
//~^ ERROR: Variable 'c' is used before being assigned.

var y = c.bar;
//~^ ERROR: Variable 'c' is used before being assigned.

c.tar;
//~^ ERROR: Property 'tar' does not exist on type 'IC<number>'.
//~| ERROR: Variable 'c' is used before being assigned.
