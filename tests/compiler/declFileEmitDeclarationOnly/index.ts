// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileEmitDeclarationOnly.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration
//@compiler-options: emitDeclarationOnly

const Log = {
  info(msg: string) {}
}

class HelloWorld {
  constructor(private name: string) {
  }

  public hello() {
    Log.info(`Hello ${this.name}`);
  }
}
