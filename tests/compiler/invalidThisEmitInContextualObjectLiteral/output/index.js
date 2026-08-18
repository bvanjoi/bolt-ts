// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/invalidThisEmitInContextualObjectLiteral.ts`, Apache-2.0 License
class TestController {
  m(def) {}
  p = this.m({
      p1: (e) => {},
    p2: () => ((vvvvvvvvv) => (this))    
  });
}