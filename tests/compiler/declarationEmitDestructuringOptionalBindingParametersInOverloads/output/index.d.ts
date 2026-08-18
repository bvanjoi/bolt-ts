declare function foo([x, y, z]?: [string, number, boolean]): any;
declare function foo(...rest: any[]): void;
declare function foo2({x, y, z}?: {
  x: string;
  y: number;
  z: boolean;
}): any;
declare function foo2(...rest: any[]): void;
