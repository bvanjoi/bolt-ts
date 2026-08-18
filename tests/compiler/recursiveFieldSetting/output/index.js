// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveFieldSetting.ts`, Apache-2.0 License
class Recursive1 {
  constructor(parent) {}
  depth = this.parent ? this.parent.depth + 1 : 0;
}
class Recursive2 {
  parent;
  depth = this.parent.depth;
}
class Recursive3 {
  parent;
  depth = this.parent.alpha;
  alpha = 0;
}