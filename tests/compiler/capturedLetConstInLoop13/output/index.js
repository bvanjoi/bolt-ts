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