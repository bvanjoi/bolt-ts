// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unknownLikeUnionObjectFlagsNotPropagated.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit
//@compiler-options: lib=[esnext]

type MyType = {} | null | undefined;

const myVar: MyType = null as MyType;

myVar?.toLocaleString;
myVar;

async function myUnusedFunction() {
    const fetch1 = Promise.resolve(['hello', 'world']);
    const [data1] = await Promise.all([fetch1]);
    data1.length;
}