// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/prototypeOnConstructorFunctions.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

interface I1 {
    const: new (options?, element?) => any;
}


var i: I1;


i.const.prototype.prop = "yo";
