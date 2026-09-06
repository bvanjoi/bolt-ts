// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/initializePropertiesWithRenamedLet.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

var x0;
if (true) {
    let x0;
    var obj1 = { x0: x0 };
    var obj2 = { x0 };
}

var x, y, z;
if (true) {
    let { x: x } = { x: 0 };
    let { y } = { y: 0 };
    let z;
    ({ z: z } = { z: 0 });
    ({ z } = { z: 0 });
}
