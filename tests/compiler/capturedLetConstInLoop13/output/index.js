// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/capturedLetConstInLoop13.ts`, Apache-2.0 License
class Main {
  constructor() {this.register('a', 'b', 'c');}
  register(...names) {
    for ( var name of names) {
      this.bar({
              [name + '.a']: () => {
          this.foo(name);
        }        
      });
    }
  }
  bar(a) {}
  foo(name) {}
}
new Main();