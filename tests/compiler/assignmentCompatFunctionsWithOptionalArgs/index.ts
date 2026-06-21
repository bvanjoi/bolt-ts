// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentCompatForEnums.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo(x: { id: number; name?: string; }): void;
//~^ ERROR: Function implementation is missing or not immediately following the declaration.
foo({ id: 1234 });                 // Ok
foo({ id: 1234, name: "hello" });  // Ok
foo({ id: 1234, name: false });    // Error, name of wrong type
//~^ ERROR: Type 'false' is not assignable to type 'string'.
foo({ name: "hello" });            // Error, id required but missing
//~^ ERROR: Property 'id' is missing.
