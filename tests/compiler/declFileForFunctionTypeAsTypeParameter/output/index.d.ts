declare class X <T>{}
declare class C  extends X<() => number> {}
interface I extends X<() => number> {}
