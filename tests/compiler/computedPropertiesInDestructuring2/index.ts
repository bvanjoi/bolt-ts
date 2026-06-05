// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/computedPropertiesInDestructuring2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

let foo2 = () => "bar";
let {[foo2()]: bar3} = {};
//~^ ERROR: Type '{ }' has no matching index signature for type 'string'.