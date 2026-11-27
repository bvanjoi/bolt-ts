// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/innerAliases2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace _provider {
                export class UsefulClass {
                                public foo() {
                                }
                }
}

namespace consumer {
                import provider = _provider;
                
                var g:provider.UsefulClass= null;
                //~^ ERROR: Type 'null' is not assignable to type '_provider.UsefulClass'.
                
                function use():provider.UsefulClass { 
                                var p2:provider.UsefulClass= new provider.UsefulClass();
                                return p2; 
                }
}

