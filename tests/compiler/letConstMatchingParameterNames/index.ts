// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/letConstMatchingParameterNames.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@compiler-options: lib=[es5]
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

let parent = true;
const parent2 = true;
declare function use(a: any);

function a() {
    
    let parent = 1;
    const parent2 = 2;

    function b(parent: string, parent2: number) {
        use(parent);
        use(parent2);
    }
}
