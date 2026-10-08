declare function doSomethingWithKeys<T>(...keys: (keyof T)[]): void;
declare var utilityFunctions: { doSomethingWithKeys: <T>(...keys: (keyof T)[]) => void; };
