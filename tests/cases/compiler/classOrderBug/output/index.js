// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/classOrderBug.ts`, Apache-2.0 License
class bar {
  baz
  constructor() {this.baz = new foo();}
}
class baz {}
class foo extends baz {}