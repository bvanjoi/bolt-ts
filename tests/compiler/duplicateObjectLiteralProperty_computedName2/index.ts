// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/duplicateObjectLiteralProperty_computedName2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
const n = 1;
const s = "s";
enum E1 { A = "ENUM_KEY" }
enum E2 { B }

const t1 = {
    [n]: 1,
    [n]: 1, // duplicate
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
}

const t2 = {
    [s]: 1,
    [s]: 1, // duplicate
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
}

const t3 = {
    [E1.A]: 1,
    [E1.A]: 1, // duplicate
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
}

const t4 = {
    [E2.B]: 1,
    [E2.B]: 1, // duplicate
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
}

const t5 = {
  [E1.A]: 1,
  "ENUM_KEY": 1, // duplicate
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
}

const t6 = {
  "ENUM_KEY": 1, // duplicate
  [E1.A]: 1,
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
}