// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/nestedBlockScopedBindings15.ts`, Apache-2.0 License

//@compiler-options: target=es2015

for (; false;) {
    {
        let x;
        () => x;
    }
}

for (; false;) {
    {
        let y;
        y = 1;
    }
}

for (; false;) {
    switch (1){
        case 1:
            let z0;
            () => z0;
            break;
    }
}

for (; false;) {
    switch (1){
        case 1:
            let z;
            z = 1;
            break;
    }
}