// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/checkSwitchStatementIfCaseTypeIsString.ts`, Apache-2.0 License
class A {
  doIt(x) {
    x.forEach((v) => {
      switch (v) {
        case 'test':
          use(this);
        
      }
    });
  }
}