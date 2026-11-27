export let b: {
    __foo: number
}

b = {
    ___foo: 100,
    //~^ ERROR: Object literal may only specify known properties, and '___foo' does not exist in type '{ __foo: number; }'.
}

