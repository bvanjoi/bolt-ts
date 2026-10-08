// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedLocalsAndObjectSpread2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es5]
//@compiler-options: noUnusedLocals

declare let props: any;
const {
    children, // here!
    active: _a, // here!
  ...rest
  //~^ ERROR: 'rest' is declared but its value is never read.
} = props;

function foo() {
  //~^ ERROR: 'foo' is declared but its value is never read.
    const {
        children,
        active: _a,
        ...rest
  //~^ ERROR: 'rest' is declared but its value is never read.
    } = props;
}

export const asdf = 123;