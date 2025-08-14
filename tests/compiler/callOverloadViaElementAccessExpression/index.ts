class C {
  foo(x: number): number;
  foo(x: string): string;
  foo(x: any): any {
      return null;
  }
}

var c = new C();
var r: string = c['foo'](1);
//~^ ERROR: Type 'number' is not assignable to type 'string'.
var r2: number = c['foo']('');
//~^ ERROR: Type 'string' is not assignable to type 'number'.
