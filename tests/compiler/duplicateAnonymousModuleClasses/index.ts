// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/duplicateAnonymousModuleClasses.ts`, Apache-2.0 License

module F {

    class Helper {

    }

}


module F {
    
    // Should not be an error
    class Helper {

    }

}

module Foo {

    class Helper {

    }

}


module Foo {
    
    // Should not be an error
    class Helper {

    }

}

module Gar {
    module Foo {

        class Helper {

        }

    }


    module Foo {
    
        // Should not be an error
        class Helper {

        }

    }
}
