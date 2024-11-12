class C {
    static foo: string;

    bar() {
        let k = foo;
        //~^ ERROR: Cannot find name 'foo'.
    }
}
