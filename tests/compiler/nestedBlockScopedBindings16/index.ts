// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/nestedBlockScopedBindings16.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var x;
for (; false;) {
    {
        let x;
        () => x;
    }
}

var y;
for (; false;) {
    {
        let y;
        y = 1;
    }
}

var z0;
for (; false;) {
    switch (1){
        case 1:
            let z0;
            () => z0;
            break;
    }
}

var z;
for (; false;) {
    switch (1){
        case 1:
            let z;
            z = 1;
            break;
    }
}