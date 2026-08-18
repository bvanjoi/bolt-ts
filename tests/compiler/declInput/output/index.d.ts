interface bar {}
declare class bar {
  f(): string;
  g(): { a: bar; b: any; c: any; };
  h(x: number, y: any, z: string): void;
}
