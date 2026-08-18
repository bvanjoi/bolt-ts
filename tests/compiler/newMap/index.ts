// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/newMap.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es6]

new Map<string>();
//~^ ERROR: No overload expects 1 type arguments, but overloads do exist that expect either 0 or 2 type arguments.
new WeakMap<object>();