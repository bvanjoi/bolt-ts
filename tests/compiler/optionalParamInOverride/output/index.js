// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/optionalParamInOverride.ts`, Apache-2.0 License
class Z {
  func() {}
}
class Y extends Z {
  func(value) {}
}