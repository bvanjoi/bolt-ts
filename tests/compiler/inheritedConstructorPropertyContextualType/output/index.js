// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/inheritedConstructorPropertyContextualType.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@run-fail
class Assignment extends Base {
  constructor() {super();this.state = {
          version: 2      
    };}
}