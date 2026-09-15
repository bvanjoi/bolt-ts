// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/errorElaboration.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

// Repro for #5712
interface Ref<T> {
    prop: T;
}
interface Container<T> {
    m1: Container<Ref<T>>;
    m2: T;
}
declare function foo(x: () => Container<Ref<number>>): void;
declare let a: () => Container<Ref<string>>;
foo(a);
//~^ ERROR: Argument of type '() => Container<Ref<string>>' is not assignable to parameter of type '() => Container<Ref<number>>'.

// Repro for #25498

function test(): {[A in "foo"]: A} {
  return {foo: "bar"};
  //~^ ERROR: Type '"bar"' is not assignable to type '"foo"'.
}

// Repro for #32358

const foo = { bar: 'a' };
//~^ ERROR: Duplicate identifier 'foo'.
const x = ({ [foo.bar]: c }) => undefined;
//~^ ERROR: Property 'bar' does not exist on type '(x: () => Container<Ref<number>>) => void'.
//~| ERROR: Type 'error' cannot be used as an index type.
//~| ERROR: Type 'error' cannot be used as an index type.
