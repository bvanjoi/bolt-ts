interface a0 {
  (): string;
  (a: any, b: any, c?: string): number;
  new (): string;
  new (s: string): any;
  [n:number]: () => string;
  [s:string]: any;
  p1;
  p2: string;
  p3?;
  p4?: number;
  p5: (s: number) => string;
  f1(): any;
  f2?(): any;
  f3(a: string): number;
  f4?(s: number): string;
}
interface a1 {
  [n:number]: number;
}
interface a2 {
  [s:string]: number;
}
interface a {}
interface b extends a {}
interface c extends a, b {}
interface d extends a {}
declare class c1  implements a {}
declare var instance2: c1;
