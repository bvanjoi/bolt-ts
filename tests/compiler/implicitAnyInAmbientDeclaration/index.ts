// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/implicitAnyInAmbientDeclaration.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

namespace Test {
    declare class C {
        public publicMember;  // this should be an error
        //~^ ERROR: Member 'publicMember' implicitly has an 'any' type.
        private privateMember;  // this should not be an error

        public publicFunction(x);  // this should be an error
        //~^ ERROR: 'publicFunction', which lacks return-type annotation, implicitly has an 'any' return type.
        //~| ERROR: Parameter 'x' implicitly has an 'any' type.
        private privateFunction(privateParam);  // this should not be an error
        private constructor(privateParam);
    }
}