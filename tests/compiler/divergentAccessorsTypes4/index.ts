// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/divergentAccessorsTypes4.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

class One {
  get prop1(): string { return ""; }
  set prop1(s: string | number) { }

  prop2: number;
  //~^ ERROR: Property 'prop2' has no initializer and is not definitely assigned in the constructor.
}

class Two {
  get prop1(): "hello" { return "hello"; }
  set prop1(s: "hello" | number) { }

  get prop2(): string { return ""; }
  set prop2(s: string | 42) { }

}

declare const i: One & Two;

// "hello"
i.prop1;
// number | "hello"
i.prop1 = 42;
i.prop1 = "hello";

// never
i.prop2;
// 42
i.prop2 = 42;
i.prop2 = "hello"; // error
//~^ ERROR: Type 'string' is not assignable to type '42'.
