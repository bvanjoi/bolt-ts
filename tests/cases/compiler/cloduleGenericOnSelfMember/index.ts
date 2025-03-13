// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/cloduleGenericOnSelfMember.ts`, Apache-2.0 License

class ServiceBase<T> {
  field: T;
}
class Service extends ServiceBase<typeof Service.Base> {
}
namespace Service {
  export const Base = {
      name: "1",
      value: 5
  };
}