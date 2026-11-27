// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/redeclareParameterInCatchBlock.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@compiler-options: target=es6

try {

} catch(e) {
    const e = null;
    //~^ ERROR: Cannot redeclare identifier 'e' in catch clause.
}

try {

} catch(e) {
    let e;
    //~^ ERROR: Cannot redeclare identifier 'e' in catch clause.
}

try {

} catch ([a, b]) {
    const [c, b] = [0, 1];
    //~^ ERROR: Cannot redeclare identifier 'b' in catch clause.
}

try {

} catch ({ a: x, b: x }) {
    //~^ ERROR: Cannot redeclare block-scoped variable 'x'.
}

try {

} catch(e) {
    function test() {
        let e;
    }
}
