// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleVisibilityTest1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es5]

declare var console: any;
 
var x = 1;
namespace M {
    export var x = 2;
    console.log(x); // 2
}
 
namespace M {
    console.log(x); // 2
}
 
namespace M {
    var x = 3;
    console.log(x); // 3
}
