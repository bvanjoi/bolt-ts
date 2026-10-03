// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classExpressionWithStaticPropertiesES63.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@compiler-options: lib=[es2015]

declare var console: any;
const arr: {y(): number}[] = [];
for (let i = 0; i < 3; i++) {
    arr.push(class C {
        static x = i;
        static y = () => C.x * 2;
    });
}
arr.forEach(C => console.log(C.y()));