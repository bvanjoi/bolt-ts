// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/targetTypeObjectLiteral.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var z: { x: number; y: (w:string)=>number;} = {

    x: 12,

    y: function(w) {

        return 0;

    }

}