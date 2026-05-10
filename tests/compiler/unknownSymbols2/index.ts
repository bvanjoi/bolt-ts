// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/unknownSymbols2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

namespace M {
    var x: asdf;
    //~^ ERROR: Cannot find name 'asdf'.
    var y = x + asdf;
    //~^ ERROR: Cannot find name 'asdf'.
    var z = <asdf>x; // should be an error
    //~^ ERROR: Cannot find name 'asdf'.
    if (asdf) {
    //~^ ERROR: Cannot find name 'asdf'.
    }
    else if (qwerty) {
    //~^ ERROR: Cannot find name 'qwerty'.
    }

    try {
    }
    catch (asdf) { // no error
    }

    switch (asdf) {
    //~^ ERROR: Cannot find name 'asdf'.
        case qwerty:
    //~^ ERROR: Cannot find name 'qwerty'.
            break;
        default:
            break;
    }

    var a = () => asdf;
    //~^ ERROR: Cannot find name 'asdf'.
    var b = (asdf) => { return qwerty };
    //~^ ERROR: Cannot find name 'qwerty'.

    namespace N {
        var x = 1;
    }
    import c = N;
    import d = asdf;
    //~^ ERROR: Cannot find name 'asdf'.
}