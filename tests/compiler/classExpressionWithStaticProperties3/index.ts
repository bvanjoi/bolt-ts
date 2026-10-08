// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classExpressionWithStaticProperties3.ts`, Apache-2.0 License

//@[target=es5]     compiler-options: target=es5
//@[target=es2015]  compiler-options: target=es2015
//@compiler-options: lib=[es5]

declare var console: any;
const arr: {y(): number}[] = [];
for (let i = 0; i < 3; i++) {
    arr.push(class C {
        static x = i;
        static y = () => C.x * 2;
    });
}
arr.forEach(C => console.log(C.y()));