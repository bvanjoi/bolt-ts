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