// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/callConstructAssignment.ts`, Apache-2.0 License

var foo:{ ( ):void; }

var bar:{ new ( ):any; }

foo = bar; // error
//~^ ERROR: Type 'new () => any' is not assignable to type '() => void'.
bar = foo; // error
//~^ ERROR: Type '() => void' is not assignable to type 'new () => any'.
