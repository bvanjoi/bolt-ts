// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parameterPropertyInConstructorWithPrologues.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class C {}
class Foo1 {
  constructor(A) {'ngInject1';}
}
class Foo2 {
  constructor(A, B) {'ngInject1';
    'ngInject2';}
}
class Foo3 {
  constructor(A, B, C) {'ngInject1';
    'ngInject2';}
}
class Foo4 {
  constructor(A) {'ngInject1';
    console.log('hi');}
}
class Foo5 {
  constructor(A, B) {'ngInject1';
    'ngInject2';
    console.log('hi');}
}
class Foo6 {
  constructor(A, B, C) {'ngInject1';
    'ngInject2';
    console.log('hi');}
}
class Foo7 extends C {
  constructor(member) {'ngInject1';
    super();console.log('hi');}
}
class Foo8 extends C {
  constructor(member) {'ngInject1';
    super();this.m();
    console.log('hi');}
  m() {}
}
class Foo9 extends C {
  constructor() {'ngInject1';
    'ngInject2';
    super();this.m();
    console.log('hi');}
  m() {}
}