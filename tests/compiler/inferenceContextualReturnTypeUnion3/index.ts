// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferenceContextualReturnTypeUnion3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit
//@compiler-options: lib=[esnext]

declare function deprecate<T extends Function>(
  fn: T,
  msg: string,
  code?: string,
): T;

const soonFrozenObjectDeprecation = <T extends object>(
  obj: T,
  name: string,
  code: string,
  note = "",
): T => {
  const message = `${name} will be frozen in future, all modifications are deprecated.${
    note && `\n${note}`
  }`;
  return new Proxy(obj, {
    set: deprecate(
      (target, property, value, receiver) =>
        Reflect.set(target, property, value, receiver),
      message,
      code,
    ),
    defineProperty: deprecate(
      (target, property, descriptor) =>
        Reflect.defineProperty(target, property, descriptor),
      message,
      code,
    ),
    deleteProperty: deprecate(
      (target, property) => Reflect.deleteProperty(target, property),
      message,
      code,
    ),
    setPrototypeOf: deprecate(
      (target, proto) => Reflect.setPrototypeOf(target, proto),
      message,
      code,
    ),
  });
};