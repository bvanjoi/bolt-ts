export function mkdirSync(path: string, mode?: number): void;
//~^ ERROR: This overload signature is not compatible with its implementation signature.
//~| ERROR: This overload signature is not compatible with its implementation signature.
//~| ERROR: This overload signature is not compatible with its implementation signature.
export function mkdirSync(path: string, mode?: string): void {}