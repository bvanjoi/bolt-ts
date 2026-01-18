// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/blockScopedBindingUsedBeforeDef.ts`, Apache-2.0 License

// 1:
for (let {[a]: a} of [{ }]) continue;
//~^ ERROR: Block-scoped variable 'a' used before its declaration.
//~| ERROR: Block-scoped variable 'a' used before its declaration.
//~| ERROR: Type 'any' cannot be used as an index type.

// // 2:
for (let {[a]: a} = { }; false; ) continue;
//~^ ERROR: Block-scoped variable 'a' used before its declaration.
//~| ERROR: Block-scoped variable 'a' used before its declaration.
//~| ERROR: Type 'any' cannot be used as an index type.

// 3:
let {[b]: b} = { };
//~^ ERROR: Block-scoped variable 'b' used before its declaration.
//~| ERROR: Block-scoped variable 'b' used before its declaration.
//~| ERROR: Type 'any' cannot be used as an index type.