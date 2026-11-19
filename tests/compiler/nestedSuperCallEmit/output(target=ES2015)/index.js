// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nestedSuperCallEmit.ts`, Apache-2.0 License
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
// https://github.com/microsoft/TypeScript/issues/55646
class Foo {
  constructor(shouldThrow) {if (shouldThrow) {
      throw new Error('Please retry');
    } else {
      console.log('OK');
    }
    }
}
class Bar extends Foo {
  constructor() {try {
      super(true);
    } catch (e) {
      console.log('Error: ' + (e).message);
      // retry
      super(false);
    }}
}
new Bar();