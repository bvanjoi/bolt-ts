// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/errorsOnUnionsOfOverlappingObjects01.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Foo {
    a: string;
    b: number;
};

interface Bar {
    b: string;
}

interface Other {
    totallyUnrelatedProperty: number;
}

export let x = { a: '', b: '' };

declare function f(x: Foo | Other): any;

f(x);
//~^ ERROR: Argument of type '{ a: string; b: string; }' is not assignable to parameter of type 'Foo | Other'.
f({ a: '', b: '' })
//~^ ERROR: Type 'string' is not assignable to type 'number'.

declare function g(x: Bar | Other): any;

g(x);
g({ a: '', b: '' })
//~^ ERROR: Object literal may only specify known properties, and 'a' does not exist in type 'Bar | Other'.

declare function h(x: Foo | Bar | Other): any;

h(x);
h({ a: '', b: '' })

interface CatDog { cat: any, dog: any }
interface ManBearPig { man: any, bear: any, pig: any }
interface Platypus { platypus: any }

type ExoticAnimal =
    | CatDog
    | ManBearPig
    | Platypus;

declare function addToZoo(animal: ExoticAnimal): void;

addToZoo({ dog: "Barky McBarkface" });
//~^ ERROR: Argument of type '{ dog: string; }' is not assignable to parameter of type 'ExoticAnimal'.
addToZoo({ man: "Manny", bear: "Coffee" });
//~^ ERROR: Argument of type '{ man: string; bear: string; }' is not assignable to parameter of type 'ExoticAnimal'.

const manBeer = { man: "Manny", beer: "Coffee" };
addToZoo({ man: "Manny", beer: "Coffee" });
//~^ ERROR: Object literal may only specify known properties, and 'beer' does not exist in type 'ExoticAnimal'.
addToZoo(manBeer);
//~^ ERROR: Argument of type '{ man: string; beer: string; }' is not assignable to parameter of type 'ExoticAnimal'.