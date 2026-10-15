// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/strictFunctionTypesErrors.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

export {}

declare let f1: (x: Object) => Object;
declare let f2: (x: Object) => string;
declare let f3: (x: string) => Object;
declare let f4: (x: string) => string;

f1 = f2;  // Ok
f1 = f3;  // Error
//~^ ERROR: Type '(x: string) => Object' is not assignable to type '(x: Object) => Object'.
f1 = f4;  // Error
//~^ ERROR: Type '(x: string) => string' is not assignable to type '(x: Object) => Object'.

f2 = f1;  // Error
//~^ ERROR: Type '(x: Object) => Object' is not assignable to type '(x: Object) => string'.
f2 = f3;  // Error
//~^ ERROR: Type '(x: string) => Object' is not assignable to type '(x: Object) => string'.
f2 = f4;  // Error
//~^ ERROR: Type '(x: string) => string' is not assignable to type '(x: Object) => string'.

f3 = f1;  // Ok
f3 = f2;  // Ok
f3 = f4;  // Ok

f4 = f1;  // Error
//~^ ERROR: Type '(x: Object) => Object' is not assignable to type '(x: string) => string'.
f4 = f2;  // Ok
f4 = f3;  // Error
//~^ ERROR: Type '(x: string) => Object' is not assignable to type '(x: string) => string'.

type Func<T, U> = (x: T) => U;

declare let g1: Func<Object, Object>;
declare let g2: Func<Object, string>;
declare let g3: Func<string, Object>;
declare let g4: Func<string, string>;

g1 = g2;  // Ok
g1 = g3;  // Error
//~^ ERROR: Type 'Func<string, Object>' is not assignable to type 'Func<Object, Object>'.
g1 = g4;  // Error
//~^ ERROR: Type 'Func<string, string>' is not assignable to type 'Func<Object, Object>'.

g2 = g1;  // Error
//~^ ERROR: Type 'Func<Object, Object>' is not assignable to type 'Func<Object, string>'.
g2 = g3;  // Error
//~^ ERROR: Type 'Func<string, Object>' is not assignable to type 'Func<Object, string>'.
g2 = g4;  // Error
//~^ ERROR: Type 'Func<string, string>' is not assignable to type 'Func<Object, string>'.

g3 = g1;  // Ok
g3 = g2;  // Ok
g3 = g4;  // Ok

g4 = g1;  // Error
//~^ ERROR: Type 'Func<Object, Object>' is not assignable to type 'Func<string, string>'.
g4 = g2;  // Ok
g4 = g3;  // Error
//~^ ERROR: Type 'Func<string, Object>' is not assignable to type 'Func<string, string>'.

declare let h1: Func<Func<Object, void>, Object>;
declare let h2: Func<Func<Object, void>, string>;
declare let h3: Func<Func<string, void>, Object>;
declare let h4: Func<Func<string, void>, string>;

h1 = h2;  // Ok
h1 = h3;  // Ok
h1 = h4;  // Ok

h2 = h1;  // Error
//~^ ERROR: Type 'Func<Func<Object, void>, Object>' is not assignable to type 'Func<Func<Object, void>, string>'.
h2 = h3;  // Error
//~^ ERROR: Type 'Func<Func<string, void>, Object>' is not assignable to type 'Func<Func<Object, void>, string>'.
h2 = h4;  // Ok

h3 = h1;  // Error
//~^ ERROR: Type 'Func<Func<Object, void>, Object>' is not assignable to type 'Func<Func<string, void>, Object>'.
h3 = h2;  // Error
//~^ ERROR: Type 'Func<Func<Object, void>, string>' is not assignable to type 'Func<Func<string, void>, Object>'.
h3 = h4;  // Ok

h4 = h1;  // Error
//~^ ERROR: Type 'Func<Func<Object, void>, Object>' is not assignable to type 'Func<Func<string, void>, string>'.
h4 = h2;  // Error
//~^ ERROR: Type 'Func<Func<Object, void>, string>' is not assignable to type 'Func<Func<string, void>, string>'.
h4 = h3;  // Error
//~^ ERROR: Type 'Func<Func<string, void>, Object>' is not assignable to type 'Func<Func<string, void>, string>'.

declare let i1: Func<Object, Func<Object, void>>;
declare let i2: Func<Object, Func<string, void>>;
declare let i3: Func<string, Func<Object, void>>;
declare let i4: Func<string, Func<string, void>>;

i1 = i2;  // Error
//~^ ERROR: Type 'Func<Object, Func<string, void>>' is not assignable to type 'Func<Object, Func<Object, void>>'.
i1 = i3;  // Error
//~^ ERROR: Type 'Func<string, Func<Object, void>>' is not assignable to type 'Func<Object, Func<Object, void>>'.
i1 = i4;  // Error
//~^ ERROR: Type 'Func<string, Func<string, void>>' is not assignable to type 'Func<Object, Func<Object, void>>'.

i2 = i1;  // Ok
i2 = i3;  // Error
//~^ ERROR: Type 'Func<string, Func<Object, void>>' is not assignable to type 'Func<Object, Func<string, void>>'.
i2 = i4;  // Error
//~^ ERROR: Type 'Func<string, Func<string, void>>' is not assignable to type 'Func<Object, Func<string, void>>'.

i3 = i1;  // Ok
i3 = i2;  // Error
//~^ ERROR: Type 'Func<Object, Func<string, void>>' is not assignable to type 'Func<string, Func<Object, void>>'.
i3 = i4;  // Error
//~^ ERROR: Type 'Func<string, Func<string, void>>' is not assignable to type 'Func<string, Func<Object, void>>'.

i4 = i1;  // Ok
i4 = i2;  // Ok
i4 = i3;  // Ok

interface Animal { animal: void }
interface Dog extends Animal { dog: void }
interface Cat extends Animal { cat: void }

interface Comparer1<T> {
    compare(a: T, b: T): number;
}

declare let animalComparer1: Comparer1<Animal>;
declare let dogComparer1: Comparer1<Dog>;

animalComparer1 = dogComparer1;  // Ok
dogComparer1 = animalComparer1;  // Ok

interface Comparer2<T> {
    compare: (a: T, b: T) => number;
}

declare let animalComparer2: Comparer2<Animal>;
declare let dogComparer2: Comparer2<Dog>;

animalComparer2 = dogComparer2;  // Error
//~^ ERROR: Property 'dog' is missing.
dogComparer2 = animalComparer2;  // Ok

// Crate<T> is invariant in --strictFunctionTypes mode

interface Crate<T> {
    item: T;
    onSetItem: (item: T) => void;
}

declare let animalCrate: Crate<Animal>;
declare let dogCrate: Crate<Dog>;

// Errors below should elaborate the reason for invariance

animalCrate = dogCrate;  // Error
//~^ ERROR: Property 'dog' is missing.
dogCrate = animalCrate;  // Error
//~^ ERROR: Property 'dog' is missing.
//~| ERROR: Property 'dog' is missing.

// Verify that callback parameters are strictly checked

declare let fc1: (f: (x: Animal) => Animal) => void;
declare let fc2: (f: (x: Dog) => Dog) => void;
fc1 = fc2;  // Error
//~^ ERROR: Property 'dog' is missing.
fc2 = fc1;  // Error
//~^ ERROR: Type '(f: (x: Animal) => Animal) => void' is not assignable to type '(f: (x: Dog) => Dog) => void'.

// Verify that callback parameters aren't loosely checked when types
// originate in method declarations

namespace n1 {
    class Foo {
        static f1(x: Animal): Animal { throw "wat"; }
        static f2(x: Dog): Animal { throw "wat"; };
    }
    declare let f1: (cb: typeof Foo.f1) => void;
    declare let f2: (cb: typeof Foo.f2) => void;
    f1 = f2;
    f2 = f1;  // Error
    //~^ ERROR: Type '(cb: (x: Animal) => Animal) => void' is not assignable to type '(cb: (x: Dog) => Animal) => void'.
}

namespace n2 {
    type BivariantHack<Input, Output> = { foo(x: Input): Output }["foo"];
    declare let f1: (cb: BivariantHack<Animal, Animal>) => void;
    declare let f2: (cb: BivariantHack<Dog, Animal>) => void;
    f1 = f2;
    f2 = f1;  // Error
    //~^ ERROR: Type '(cb: (x: Animal) => Animal) => void' is not assignable to type '(cb: (x: Dog) => Animal) => void'.
}