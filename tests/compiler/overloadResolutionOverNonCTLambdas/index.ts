// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/overloadResolutionOverNonCTLambdas.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

namespace Bugs {
  class A {
  }
  
  // replace(searchValue: RegExp, replaceValue: (substring: string, ...args: any[]) => string): string;
  function bug2(message:string, ...args:any[]):string {
    var result= message.replace(/\{(\d+)\}/g, function(match, ...rest) {
      var index= rest[0];
      return typeof args[index] !== 'undefined'
        ? args[index]
        : match;
    });
    return result;
  }
}

function bug3(f:(x:string)=>string) { return f("s") }

function fprime(x:string):string { return x; }

bug3(fprime);

bug3(function(x:string):string { return x; });