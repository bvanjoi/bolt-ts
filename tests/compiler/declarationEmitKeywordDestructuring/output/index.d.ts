type P = {
  enum: boolean;
  function: boolean;
  abstract: boolean;
  async: boolean;
  await: boolean;
  one: boolean;
};
declare function f1({enum: _enum, ...rest}: P): { function: boolean; abstract: boolean; async: boolean; await: boolean; one: boolean; };
declare function f2({function: _function, ...rest}: P): { enum: boolean; abstract: boolean; async: boolean; await: boolean; one: boolean; };
declare function f3({abstract: _abstract, ...rest}: P): { enum: boolean; function: boolean; async: boolean; await: boolean; one: boolean; };
declare function f4({async: _async, ...rest}: P): { enum: boolean; function: boolean; abstract: boolean; await: boolean; one: boolean; };
declare function f5({await: _await, ...rest}: P): { enum: boolean; function: boolean; abstract: boolean; async: boolean; one: boolean; };
