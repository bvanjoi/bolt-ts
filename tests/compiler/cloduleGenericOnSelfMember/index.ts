// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/cloduleGenericOnSelfMember.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class ServiceBase<T> {
  field: T;
  //~^ ERROR: Property 'field' has no initializer and is not definitely assigned in the constructor.
}
class Service extends ServiceBase<typeof Service.Base> {
}
namespace Service {
  export const Base = {
      name: "1",
      value: 5
  };
}

function f(a: ServiceBase<typeof Service.Base>) {
  let b: number = a.field.name;
  //~^ ERROR: Type 'string' is not assignable to type 'number'.
}
