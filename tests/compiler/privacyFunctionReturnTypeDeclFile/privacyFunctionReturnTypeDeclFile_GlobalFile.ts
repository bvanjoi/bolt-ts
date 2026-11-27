class publicClassInGlobal {
}
interface publicInterfaceWithPublicParmeterTypesInGlobal {
    new (): publicClassInGlobal;
    (): publicClassInGlobal;
    [x: number]: publicClassInGlobal;
    myMethod(): publicClassInGlobal;
}
class publicClassWithWithPublicParmeterTypesInGlobal {
    static myPublicStaticMethod(): publicClassInGlobal {
        return null;
        //~^ ERROR: Type 'null' is not assignable to type 'publicClassInGlobal'.
    }
    private static myPrivateStaticMethod(): publicClassInGlobal {
        return null;
        //~^ ERROR: Type 'null' is not assignable to type 'publicClassInGlobal'.
    }
    myPublicMethod(): publicClassInGlobal {
        return null;
        //~^ ERROR: Type 'null' is not assignable to type 'publicClassInGlobal'.
    }
    private myPrivateMethod(): publicClassInGlobal {
        return null;
        //~^ ERROR: Type 'null' is not assignable to type 'publicClassInGlobal'.
    }
    static myPublicStaticMethod1() {
        return new publicClassInGlobal();
    }
    private static myPrivateStaticMethod1() {
        return new publicClassInGlobal();
    }
    myPublicMethod1() {
        return new publicClassInGlobal();
    }
    private myPrivateMethod1() {
        return new publicClassInGlobal();
    }
}
function publicFunctionWithPublicParmeterTypesInGlobal(): publicClassInGlobal {
    return null;
    //~^ ERROR: Type 'null' is not assignable to type 'publicClassInGlobal'.
}
function publicFunctionWithPublicParmeterTypesInGlobal1() {
    return new publicClassInGlobal();
}
declare function publicAmbientFunctionWithPublicParmeterTypesInGlobal(): publicClassInGlobal;

namespace publicModuleInGlobal {
    class privateClass {
    }

    export class publicClass {
    }

    namespace privateModule {
        class privateClass {
        }

        export class publicClass {
        }

        export interface publicInterfaceWithPrivateParmeterTypes {
            new (): privateClass;
            (): privateClass;
            [x: number]: privateClass;
            myMethod(): privateClass;
        }

        export interface publicInterfaceWithPublicParmeterTypes {
            new (): publicClass;
            (): publicClass;
            [x: number]: publicClass;
            myMethod(): publicClass;
        }

        interface privateInterfaceWithPrivateParmeterTypes {
            new (): privateClass;
            (): privateClass;
            [x: number]: privateClass;
            myMethod(): privateClass;
        }

        interface privateInterfaceWithPublicParmeterTypes {
            new (): publicClass;
            (): publicClass;
            [x: number]: publicClass;
            myMethod(): publicClass;
        }

        export class publicClassWithWithPrivateParmeterTypes {
            static myPublicStaticMethod(): privateClass {
                return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateClass'.
            }
            private static myPrivateStaticMethod(): privateClass {
                return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateClass'.
            }
            myPublicMethod(): privateClass {
                return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateClass'.
            }
            private myPrivateMethod(): privateClass {
                return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateClass'.
            }
            static myPublicStaticMethod1() {
                return new privateClass();
            }
            private static myPrivateStaticMethod1() {
                return new privateClass();
            }
            myPublicMethod1() {
                return new privateClass();
            }
            private myPrivateMethod1() {
                return new privateClass();
            }
        }

        export class publicClassWithWithPublicParmeterTypes {
            static myPublicStaticMethod(): publicClass {
                return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
            }
            private static myPrivateStaticMethod(): publicClass {
                return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
            }
            myPublicMethod(): publicClass {
                return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
            }
            private myPrivateMethod(): publicClass {
                return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
            }
            static myPublicStaticMethod1() {
                return new publicClass();
            }
            private static myPrivateStaticMethod1() {
                return new publicClass();
            }
            myPublicMethod1() {
                return new publicClass();
            }
            private myPrivateMethod1() {
                return new publicClass();
            }
        }

        class privateClassWithWithPrivateParmeterTypes {
            static myPublicStaticMethod(): privateClass {
                return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateClass'.
            }
            private static myPrivateStaticMethod(): privateClass {
                return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateClass'.
            }
            myPublicMethod(): privateClass {
                return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateClass'.
            }
            private myPrivateMethod(): privateClass {
                return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateClass'.
            }
            static myPublicStaticMethod1() {
                return new privateClass();
            }
            private static myPrivateStaticMethod1() {
                return new privateClass();
            }
            myPublicMethod1() {
                return new privateClass();
            }
            private myPrivateMethod1() {
                return new privateClass();
            }
        }

        class privateClassWithWithPublicParmeterTypes {
            static myPublicStaticMethod(): publicClass {
                return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
            }
            private static myPrivateStaticMethod(): publicClass {
                return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
            }
            myPublicMethod(): publicClass {
                return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
            }
            private myPrivateMethod(): publicClass {
                return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
            }
            static myPublicStaticMethod1() {
                return new publicClass();
            }
            private static myPrivateStaticMethod1() {
                return new publicClass();
            }
            myPublicMethod1() {
                return new publicClass();
            }
            private myPrivateMethod1() {
                return new publicClass();
            }
        }

        export function publicFunctionWithPrivateParmeterTypes(): privateClass {
            return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateClass'.
        }
        export function publicFunctionWithPublicParmeterTypes(): publicClass {
            return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
        }
        function privateFunctionWithPrivateParmeterTypes(): privateClass {
            return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateClass'.
        }
        function privateFunctionWithPublicParmeterTypes(): publicClass {
            return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
        }
        export function publicFunctionWithPrivateParmeterTypes1() {
            return new privateClass();
        }
        export function publicFunctionWithPublicParmeterTypes1() {
            return new publicClass();
        }
        function privateFunctionWithPrivateParmeterTypes1() {
            return new privateClass();
        }
        function privateFunctionWithPublicParmeterTypes1() {
            return new publicClass();
        }

        export declare function publicAmbientFunctionWithPrivateParmeterTypes(): privateClass;
        export declare function publicAmbientFunctionWithPublicParmeterTypes(): publicClass;
        declare function privateAmbientFunctionWithPrivateParmeterTypes(): privateClass;
        declare function privateAmbientFunctionWithPublicParmeterTypes(): publicClass;

        export interface publicInterfaceWithPrivateModuleParameterTypes {
            new (): privateModule.publicClass;
            (): privateModule.publicClass;
            [x: number]: privateModule.publicClass;
            myMethod(): privateModule.publicClass;
        }
        export class publicClassWithPrivateModuleParameterTypes {
            static myPublicStaticMethod(): privateModule.publicClass {
                return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
            }
            myPublicMethod(): privateModule.publicClass {
                return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
            }
            static myPublicStaticMethod1() {
                return new privateModule.publicClass();
            }
            myPublicMethod1() {
                return new privateModule.publicClass();
            }
        }
        export function publicFunctionWithPrivateModuleParameterTypes(): privateModule.publicClass {
            return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
        }
        export function publicFunctionWithPrivateModuleParameterTypes1() {
            return new privateModule.publicClass();
        }
        export declare function publicAmbientFunctionWithPrivateModuleParameterTypes(): privateModule.publicClass;

        interface privateInterfaceWithPrivateModuleParameterTypes {
            new (): privateModule.publicClass;
            (): privateModule.publicClass;
            [x: number]: privateModule.publicClass;
            myMethod(): privateModule.publicClass;
        }
        class privateClassWithPrivateModuleParameterTypes {
            static myPublicStaticMethod(): privateModule.publicClass {
                return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
            }
            myPublicMethod(): privateModule.publicClass {
                return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
            }
            static myPublicStaticMethod1() {
                return new privateModule.publicClass();
            }
            myPublicMethod1() {
                return new privateModule.publicClass();
            }
        }
        function privateFunctionWithPrivateModuleParameterTypes(): privateModule.publicClass {
            return null;
                //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
        }
        function privateFunctionWithPrivateModuleParameterTypes1() {
            return new privateModule.publicClass();
        }
        declare function privateAmbientFunctionWithPrivateModuleParameterTypes(): privateModule.publicClass;
    }

    export interface publicInterfaceWithPrivateParmeterTypes {
        new (): privateClass; // Error
        (): privateClass; // Error
        [x: number]: privateClass; // Error
        myMethod(): privateClass; // Error
    }

    export interface publicInterfaceWithPublicParmeterTypes {
        new (): publicClass;
        (): publicClass;
        [x: number]: publicClass;
        myMethod(): publicClass;
    }

    interface privateInterfaceWithPrivateParmeterTypes {
        new (): privateClass;
        (): privateClass;
        [x: number]: privateClass;
        myMethod(): privateClass;
    }

    interface privateInterfaceWithPublicParmeterTypes {
        new (): publicClass;
        (): publicClass;
        [x: number]: publicClass;
        myMethod(): publicClass;
    }

    export class publicClassWithWithPrivateParmeterTypes {
        static myPublicStaticMethod(): privateClass { // Error
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'privateClass'.
        }
        private static myPrivateStaticMethod(): privateClass {
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'privateClass'.
        }
        myPublicMethod(): privateClass { // Error
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'privateClass'.
        }
        private myPrivateMethod(): privateClass {
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'privateClass'.
        }
        static myPublicStaticMethod1() { // Error
            return new privateClass();
        }
        private static myPrivateStaticMethod1() {
            return new privateClass();
        }
        myPublicMethod1() { // Error
            return new privateClass();
        }
        private myPrivateMethod1() {
            return new privateClass();
        }
    }

    export class publicClassWithWithPublicParmeterTypes {
        static myPublicStaticMethod(): publicClass {
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'publicModuleInGlobal.publicClass'.
        }
        private static myPrivateStaticMethod(): publicClass {
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'publicModuleInGlobal.publicClass'.
        }
        myPublicMethod(): publicClass {
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'publicModuleInGlobal.publicClass'.
        }
        private myPrivateMethod(): publicClass {
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'publicModuleInGlobal.publicClass'.
        }
        static myPublicStaticMethod1() {
            return new publicClass();
        }
        private static myPrivateStaticMethod1() {
            return new publicClass();
        }
        myPublicMethod1() {
            return new publicClass();
        }
        private myPrivateMethod1() {
            return new publicClass();
        }
    }

    class privateClassWithWithPrivateParmeterTypes {
        static myPublicStaticMethod(): privateClass {
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'privateClass'.
        }
        private static myPrivateStaticMethod(): privateClass {
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'privateClass'.
        }
        myPublicMethod(): privateClass {
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'privateClass'.
        }
        private myPrivateMethod(): privateClass {
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'privateClass'.
        }
        static myPublicStaticMethod1() {
            return new privateClass();
        }
        private static myPrivateStaticMethod1() {
            return new privateClass();
        }
        myPublicMethod1() {
            return new privateClass();
        }
        private myPrivateMethod1() {
            return new privateClass();
        }
    }

    class privateClassWithWithPublicParmeterTypes {
        static myPublicStaticMethod(): publicClass {
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'publicModuleInGlobal.publicClass'.
        }
        private static myPrivateStaticMethod(): publicClass {
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'publicModuleInGlobal.publicClass'.
        }
        myPublicMethod(): publicClass {
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'publicModuleInGlobal.publicClass'.
        }
        private myPrivateMethod(): publicClass {
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'publicModuleInGlobal.publicClass'.
        }
        static myPublicStaticMethod1() {
            return new publicClass();
        }
        private static myPrivateStaticMethod1() {
            return new publicClass();
        }
        myPublicMethod1() {
            return new publicClass();
        }
        private myPrivateMethod1() {
            return new publicClass();
        }
    }

    export function publicFunctionWithPrivateParmeterTypes(): privateClass { // Error
        return null;
        //~^ ERROR: Type 'null' is not assignable to type 'privateClass'.
    }
    export function publicFunctionWithPublicParmeterTypes(): publicClass {
        return null;
        //~^ ERROR: Type 'null' is not assignable to type 'publicModuleInGlobal.publicClass'.
    }
    function privateFunctionWithPrivateParmeterTypes(): privateClass {
        return null;
        //~^ ERROR: Type 'null' is not assignable to type 'privateClass'.
    }
    function privateFunctionWithPublicParmeterTypes(): publicClass {
        return null;
        //~^ ERROR: Type 'null' is not assignable to type 'publicModuleInGlobal.publicClass'.
    }
    export function publicFunctionWithPrivateParmeterTypes1() { // Error
        return new privateClass();
    }
    export function publicFunctionWithPublicParmeterTypes1() {
        return new publicClass();
    }
    function privateFunctionWithPrivateParmeterTypes1() {
        return new privateClass();
    }
    function privateFunctionWithPublicParmeterTypes1() {
        return new publicClass();
    }

    export declare function publicAmbientFunctionWithPrivateParmeterTypes(): privateClass; // Error
    export declare function publicAmbientFunctionWithPublicParmeterTypes(): publicClass;
    declare function privateAmbientFunctionWithPrivateParmeterTypes(): privateClass;
    declare function privateAmbientFunctionWithPublicParmeterTypes(): publicClass;

    export interface publicInterfaceWithPrivateModuleParameterTypes {
        new (): privateModule.publicClass; // Error
        (): privateModule.publicClass; // Error
        [x: number]: privateModule.publicClass; // Error
        myMethod(): privateModule.publicClass; // Error
    }
    export class publicClassWithPrivateModuleParameterTypes {
        static myPublicStaticMethod(): privateModule.publicClass { // Error
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
        }
        myPublicMethod(): privateModule.publicClass { // Error
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
        }
        static myPublicStaticMethod1() { // Error
            return new privateModule.publicClass();
        }
        myPublicMethod1() { // Error
            return new privateModule.publicClass();
        }
    }
    export function publicFunctionWithPrivateModuleParameterTypes(): privateModule.publicClass { // Error
        return null;
            //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
    }
    export function publicFunctionWithPrivateModuleParameterTypes1() { // Error
        return new privateModule.publicClass();
    }
    export declare function publicAmbientFunctionWithPrivateModuleParameterTypes(): privateModule.publicClass; // Error

    interface privateInterfaceWithPrivateModuleParameterTypes {
        new (): privateModule.publicClass;
        (): privateModule.publicClass;
        [x: number]: privateModule.publicClass;
        myMethod(): privateModule.publicClass;
    }
    class privateClassWithPrivateModuleParameterTypes {
        static myPublicStaticMethod(): privateModule.publicClass {
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
        }
        myPublicMethod(): privateModule.publicClass {
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
        }
        static myPublicStaticMethod1() {
            return new privateModule.publicClass();
        }
        myPublicMethod1() {
            return new privateModule.publicClass();
        }
    }
    function privateFunctionWithPrivateModuleParameterTypes(): privateModule.publicClass {
        return null;
        //~^ ERROR: Type 'null' is not assignable to type 'privateModule.publicClass'.
    }
    function privateFunctionWithPrivateModuleParameterTypes1() {
        return new privateModule.publicClass();
    }
    declare function privateAmbientFunctionWithPrivateModuleParameterTypes(): privateModule.publicClass;
}