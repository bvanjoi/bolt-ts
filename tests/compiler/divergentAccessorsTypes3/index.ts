// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/divergentAccessorsTypes3.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

class One {
  get prop1(): string { return ""; }
  set prop1(s: string | number) { }

  get prop2(): string { return ""; }
  set prop2(s: string | number) { }

  prop3: number;
  //~^ ERROR: roperty 'prop3' has no initializer and is not definitely assigned in the constructor.

  get prop4(): string { return ""; }
  set prop4(s: string | number) { }
}

class Two {
  get prop1(): string { return ""; }
  set prop1(s: string | number) { }

  get prop2(): string { return ""; }
  set prop2(s: string) { }

  get prop3(): string { return ""; }
  set prop3(s: string | boolean) { }

  get prop4(): string { return ""; }
  set prop4(s: string | boolean) { }
}

declare const u1: One|Two;

u1.prop1 = 42;
u1.prop1 = "hello";

u1.prop2 = 42;
u1.prop2 = "hello";

u1.prop3 = 42;
u1.prop3 = "hello";
u1.prop3 = true;

u1.prop4 = 42;
u1.prop4 = "hello";
u1.prop4 = true;
