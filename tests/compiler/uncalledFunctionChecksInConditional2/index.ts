// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/uncalledFunctionChecksInConditional2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[esnext,dom]

{
  const perf = window.performance

  // Simplified
  if (
    perf &&
    perf.measure &&
    perf.clearMarks &&
    perf.clearMeasures
  ) {
    perf.measure("");
    perf.clearMarks("")
    perf.clearMeasures("")
  }

  // With ||
  if (
    perf &&
    perf.mark &&
    perf.measure || !!true
    //~^ ERROR: This condition will always return true since this function is always defined. Did you mean to call it instead?
  ) {
    perf.mark("");
  }

  // With ??
  if (
    (
      perf &&
      perf.mark &&
      perf.measure
    //~^ ERROR: This condition will always return true since this function is always defined. Did you mean to call it instead?
    ) ?? !!true
  ) {
    perf.mark("");
  }
};

// Original #49192
declare let inBrowser: boolean;
{
  let mark;
  let measure;
  const perf = inBrowser && window.performance
  /* istanbul ignore if */
  if (
    perf &&
    perf.mark &&
    perf.measure &&
    perf.clearMarks &&
    perf.clearMeasures
    //~^ ERROR: This condition will always return true since this function is always defined. Did you mean to call it instead?
  ) {
    mark = (tag) => perf.mark(tag)
    //~^ ERROR: Parameter 'tag' implicitly has an 'any' type.
    measure = (name, startTag, endTag) => {
      //~^ ERROR: Parameter 'name' implicitly has an 'any' type.
      //~| ERROR: Parameter 'startTag' implicitly has an 'any' type.
      //~| ERROR: Parameter 'endTag' implicitly has an 'any' type.
      perf.measure(name, startTag, endTag)
      perf.clearMarks(startTag)
      perf.clearMarks(endTag)
      // perf.clearMeasures(name)
    }
  }
};

let _isMobile: boolean;
function isMobile() {
  if (_isMobile === undefined) {
    const isMobileMatch =
      typeof window !== 'undefined' &&
      window.matchMedia && // no error
      window.matchMedia('(max-device-width: 680px)');
    _isMobile = isMobileMatch && isMobileMatch.matches;
  }
  return _isMobile;
}
