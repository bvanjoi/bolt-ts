// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualSignatureInstantiation2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// dot f g x = f(g(x))
var dot: <T, S>(f: (_: T) => S) => <U>(g: (_: U) => T) => (_: U) => S;
dot = <T, S>(f: (_: T) => S) => <U>(g: (_: U) => T): (r:U) => S => (x) => f(g(x));
var id: <T>(x:T) => T;
var r23 = dot(id)(id);
//~^ ERROR: Variable 'id' is used before being assigned.
//~| ERROR: Variable 'id' is used before being assigned.
//~| ERROR: Variable 'id' is used before being assigned.
//~| ERROR: Variable 'id' is used before being assigned.
//~| ERROR: Variable 'id' is used before being assigned.
//~| ERROR: Variable 'id' is used before being assigned.
//~| ERROR: Variable 'id' is used before being assigned.
//~| ERROR: Variable 'id' is used before being assigned.
