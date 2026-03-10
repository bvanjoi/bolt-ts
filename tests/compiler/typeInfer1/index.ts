// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeInfer1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface ITextWriter2 {
    Write(s:string):void;
    WriteLine(s:string):void;
}

var x: ITextWriter2 = {
    Write: function (s:string):void {},
    WriteLine: function(s:string):void {}
}

var yyyyyyyy: ITextWriter2 = {
    Moo: function() { return "cow"; }
    //~^ ERROR: Object literal may only specify known properties, and 'Moo' does not exist in type 'ITextWriter2'.
}
