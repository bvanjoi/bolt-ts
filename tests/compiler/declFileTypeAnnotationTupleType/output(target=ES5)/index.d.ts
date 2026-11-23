declare class c {}
declare namespace m {
  class c {}
  class g <T>{}
}
declare class g <T>{}
declare var k: [c, m.c];
declare var l: [c, m.c];
declare var x: [g<string>, m.g<number>, () => c];
declare var y: [g<string>, m.g<number>, () => c];
