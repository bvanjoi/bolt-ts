// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typePredicateInherit.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface A {
  method1(): this is {
    a: 1
  }
  method2(): boolean;
  method3(): this is {
    a: 1
  };
}
class B implements A {
  method1() { }      // should error
  //~^ ERROR: Property 'method1' in type 'B<B>' is not assignable to the same property in base type 'A<B>'.

  method2() { }   // should error
  //~^ ERROR: Property 'method2' in type 'B<B>' is not assignable to the same property in base type 'A<B>'.

  method3() {   // should error
  //~^ ERROR: Property 'method3' in type 'B<B>' is not assignable to the same property in base type 'A<B>'.
    return true
  }
}

class C {
  method1(): this is {
    a: 1
  } {
    return true;
  }

  method2(): this is {
    a: 1
  } {
    return true;
  }

  method3(): this is {
    a: 1
  } {
    return true;
  }
}

class D extends C {
  method1(): void {   // should error
  //~^ ERROR: Property 'method1' in type 'D<D>' is not assignable to the same property in base type 'C<D>'.
  }

  method2(): this is {  // should ok
    a: 1
  } {
    return true;
  }

  method3(): boolean {  // should error
  //~^ ERROR: Property 'method3' in type 'D<D>' is not assignable to the same property in base type 'C<D>'.
    return true;
  }
}