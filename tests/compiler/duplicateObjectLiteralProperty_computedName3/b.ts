import * as keys from "./a";

const t1 = {
    [keys.n]: 1,
    [keys.n]: 1, // duplicate
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
}

const t2 = {
    [keys.s]: 1,
    [keys.s]: 1, // duplicate
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
}

const t3 = {
    [keys.E1.A]: 1,
    [keys.E1.A]: 1, // duplicate
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
}

const t4 = {
    [keys.E2.B]: 1,
    [keys.E2.B]: 1, // duplicate
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
}
