// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/fatarrowfunctionsInFunctions.ts`, Apache-2.0 License

declare function setTimeout(expression: any, msec?: number, language?: any): number;

var messenger = {
    message: "Hello World",
    start: function() {
        var _self = this;
        setTimeout(function() {
            _self.message.toString(); 
        }, 0); 
    }
}; 
messenger.start(); 
