// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericSignatureInheritance.ts`, Apache-2.0 License

interface I {
  <T>(x: T): string;
}

interface I2 extends I { }

declare function f0<T extends unknown[] | []>(x: T): number;
declare function f0<T>(x: Iterable<T>): string;
function f0Test() {
  const a0: number = f0(['42']);
  const a1: string = f0(new Set(['42']));
  const a2: number = f0(['42'] as const);
  const a3: number = f0([]);
}

declare function f1<T>(x: Iterable<T>): string;
declare function f1<T extends unknown[] | []>(x: T): number;
function f1Test() {
  const a0: number = f1(['42']);
  //~^ ERROR: Type 'string' is not assignable to type 'number'.
  const a1: string = f1(new Set(['42']));
  const a2: number = f1(['42'] as const);
  //~^ ERROR: Type 'string' is not assignable to type 'number'.
  const a3: number = f1([]);
  //~^ ERROR: Type 'string' is not assignable to type 'number'.
}


interface A {
  f<T extends unknown[] | []>(x: T): number;
  f<T>(x: Iterable<T>): string;
}

function aTest(a: A) {
  const a0: number = a.f(['42']);
  const a1: string = a.f(new Set(['42']));
  const a2: number = a.f(['42'] as const);
  const a3: number = a.f([]);
}

interface B {
  f<T>(x: Iterable<T>): string;
  f<T extends unknown[] | []>(x: T): number;
}

function bTest(b: B) {
  const a0: number = b.f(['42']);
  //~^ ERROR: Type 'string' is not assignable to type 'number'.
  const a1: string = b.f(new Set(['42']));
  const a2: number = b.f(['42'] as const);
  //~^ ERROR: Type 'string' is not assignable to type 'number'.
  const a3: number = b.f([]);
  //~^ ERROR: Type 'string' is not assignable to type 'number'.
}

interface C {
  f(): number;
}

interface C {
  f(): string;
}

function cTest(c: C) {
  const a0: number = c.f();
  //~^ ERROR: Type 'string' is not assignable to type 'number'.
}

interface D {
  f(): string;
}

interface D {
  f(): number;
}

function dTest(d: D) {
  const a0: number = d.f();
}

interface E {
  f(b: '42'): number;
}

interface E {
  f(b: '42'): string;
}

function eTest(e: E) {
  const a0: string = e.f('42');
  //~^ ERROR: Type 'number' is not assignable to type 'string'.
}

interface F {
  f(b: '42'): string;
}

interface F {
  f(b: '42'): number;
}

function fTest(f: F) {
  const a0: number = f.f('42');
  //~^ ERROR: Type 'string' is not assignable to type 'number'.
}