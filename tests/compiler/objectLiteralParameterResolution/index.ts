// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectLiteralParameterResolution.ts`, Apache-2.0 License

//@compiler-options: target=esnext

interface Foo{
    extend<T>(target: T, ...objs: any[]): T;
    extend<T>(deep: boolean, target: T, ...objs: any[]): T;
}
declare var $: Foo;
var s = $.extend({
    type: "GET" ,
    data: "data" ,
    success: wrapSuccessCallback(requestContext, callback) ,
    //~^ ERROR: Cannot find name 'wrapSuccessCallback'.
    //~| ERROR: Cannot find name 'requestContext'.
    //~| ERROR: Cannot find name 'callback'.
    error: wrapErrorCallback(requestContext, errorCallback) ,
    //~^ ERROR: Cannot find name 'wrapErrorCallback'.
    //~| ERROR: Cannot find name 'requestContext'.
    //~| ERROR: Cannot find name 'errorCallback'.
    dataType: "json" ,
    converters: { "text json": "" },
    traditional: true ,
    timeout: 12,
    }, "");
