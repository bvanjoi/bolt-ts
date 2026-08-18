// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noAsConstNameLookup.ts`, Apache-2.0 License
//@compiler-options: target=es2015
export class FeatureRunner {
  cleaners = [];
  runFeature() {
    var objectWhichShouldBeConst = {
          flags: {},
      settings: {}      
    };
    return objectWhichShouldBeConst;
  }
  run() {
    var result = {};
    this.cleaners.forEach((c) => (c(this)));
    return result;
  }
}
export class C {
  f() {
    var one = 1;
  }
}
new C().f();