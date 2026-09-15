// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyLoopCrash.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: noImplicitAny

let foo = () => {};
let bar;
while (1) {
    bar = ~foo(...bar);
    //~[target=ES5]^ ERROR: Type 'undefined | number' is not an array type.
    //~[target=ES2015]^^ ERROR: Type 'undefined | number' must have a '[Symbol.iterator]()' method that returns an iterator.
    //~| ERROR: A spread argument must either have a tuple type or be passed to a rest parameter.
    //~| ERROR: A spread argument must either have a tuple type or be passed to a rest parameter.
}
