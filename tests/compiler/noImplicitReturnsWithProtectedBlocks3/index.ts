// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitReturnsWithProtectedBlocks3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitReturns

declare function log(s: string): void;
declare function get(): number;

function main1() : number {
  //~^ ERROR: Not all code paths return a value.
    try {
        return get();
    }
    catch(e) {
        log("in catch");
    }
}