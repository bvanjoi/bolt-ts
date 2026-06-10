// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/localImportNameVsGlobalName.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace Keyboard {
  export enum Key { UP, DOWN, LEFT, RIGHT }
}

namespace App {
  import Key = Keyboard.Key;

  export function foo(key: Key): void {}

  foo(Key.UP);
  foo(Key.DOWN);
  foo(Key.LEFT);
}