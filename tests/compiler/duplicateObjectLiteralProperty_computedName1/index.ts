// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/duplicateObjectLiteralProperty_computedName1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

const t1 = {
    1: 1,
    [1]: 0 // duplicate
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
}

const t2 = {
    1: 1,
    [+1]: 0 // duplicate
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
}

const t3 = {
    "1": 1,
    [+1]: 0 // duplicate
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
}

const t4 = {
    "+1": 1,
    [+1]: 0 // two different keys, "+1", "1"
}

const t5 = {
    "+1": 1,
    ["+1"]: 0 // duplicate
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
}

const t6 = {
    "-1": 1,
    [-1]: 0 // duplicate
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
}

const t7 = {
    "-1": 1,
    ["-1"]: 0 // duplicate
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
}

const t8 = {
    "2.71828": 1,
    [2.71828]: 0 // duplicate
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
}

const t9 = {
    "0x77": 1,
    [0x77]: 0 // two different keys
}

const t10 = {
    ".42": 1,
    [0.42]: 0 // two different keys
}