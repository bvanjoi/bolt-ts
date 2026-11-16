class publicClassInGlobal {
}
interface publicInterfaceWithPublicPropertyTypesInGlobal {
    myProperty: publicClassInGlobal;
}
class publicClassWithWithPublicPropertyTypesInGlobal {
    static myPublicStaticProperty: publicClassInGlobal;
    private static myPrivateStaticProperty: publicClassInGlobal;
    myPublicProperty: publicClassInGlobal;
    //~^ ERROR: Property 'myPublicProperty' has no initializer and is not definitely assigned in the constructor.
    private myPrivateProperty: publicClassInGlobal;
    //~^ ERROR: Property 'myPrivateProperty' has no initializer and is not definitely assigned in the constructor.
}
var publicVarWithPublicPropertyTypesInGlobal: publicClassInGlobal;
declare var publicAmbientVarWithPublicPropertyTypesInGlobal: publicClassInGlobal;

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

        export interface publicInterfaceWithPrivatePropertyTypes {
            myProperty: privateClass;
        }

        export interface publicInterfaceWithPublicPropertyTypes {
            myProperty: publicClass;
        }

        interface privateInterfaceWithPrivatePropertyTypes {
            myProperty: privateClass;
        }

        interface privateInterfaceWithPublicPropertyTypes {
            myProperty: publicClass;
        }

        export class publicClassWithWithPrivatePropertyTypes {
            static myPublicStaticProperty: privateClass;
            private static myPrivateStaticProperty: privateClass;
            myPublicProperty: privateClass;
            //~^ ERROR: Property 'myPublicProperty' has no initializer and is not definitely assigned in the constructor.
            private myPrivateProperty: privateClass;
            //~^ ERROR: Property 'myPrivateProperty' has no initializer and is not definitely assigned in the constructor.
        }

        export class publicClassWithWithPublicPropertyTypes {
            static myPublicStaticProperty: publicClass;
            private static myPrivateStaticProperty: publicClass;
            myPublicProperty: publicClass;
            //~^ ERROR: Property 'myPublicProperty' has no initializer and is not definitely assigned in the constructor.
            private myPrivateProperty: publicClass;
            //~^ ERROR: Property 'myPrivateProperty' has no initializer and is not definitely assigned in the constructor.
        }

        class privateClassWithWithPrivatePropertyTypes {
            static myPublicStaticProperty: privateClass;
            private static myPrivateStaticProperty: privateClass;
            myPublicProperty: privateClass;
            //~^ ERROR: Property 'myPublicProperty' has no initializer and is not definitely assigned in the constructor.
            private myPrivateProperty: privateClass;
            //~^ ERROR: Property 'myPrivateProperty' has no initializer and is not definitely assigned in the constructor.
        }

        class privateClassWithWithPublicPropertyTypes {
            static myPublicStaticProperty: publicClass;
            private static myPrivateStaticProperty: publicClass;
            myPublicProperty: publicClass;
            //~^ ERROR: Property 'myPublicProperty' has no initializer and is not definitely assigned in the constructor.
            private myPrivateProperty: publicClass;
            //~^ ERROR: Property 'myPrivateProperty' has no initializer and is not definitely assigned in the constructor.
        }

        export var publicVarWithPrivatePropertyTypes: privateClass;
        export var publicVarWithPublicPropertyTypes: publicClass;
        var privateVarWithPrivatePropertyTypes: privateClass;
        var privateVarWithPublicPropertyTypes: publicClass;

        export declare var publicAmbientVarWithPrivatePropertyTypes: privateClass;
        export declare var publicAmbientVarWithPublicPropertyTypes: publicClass;
        declare var privateAmbientVarWithPrivatePropertyTypes: privateClass;
        declare var privateAmbientVarWithPublicPropertyTypes: publicClass;

        export interface publicInterfaceWithPrivateModulePropertyTypes {
            myProperty: privateModule.publicClass;
        }
        export class publicClassWithPrivateModulePropertyTypes {
            static myPublicStaticProperty: privateModule.publicClass;
            myPublicProperty: privateModule.publicClass;
            //~^ ERROR: Property 'myPublicProperty' has no initializer and is not definitely assigned in the constructor.
        }
        export var publicVarWithPrivateModulePropertyTypes: privateModule.publicClass;
        export declare var publicAmbientVarWithPrivateModulePropertyTypes: privateModule.publicClass;

        interface privateInterfaceWithPrivateModulePropertyTypes {
            myProperty: privateModule.publicClass;
        }
        class privateClassWithPrivateModulePropertyTypes {
            static myPublicStaticProperty: privateModule.publicClass;
            myPublicProperty: privateModule.publicClass;
            //~^ ERROR: Property 'myPublicProperty' has no initializer and is not definitely assigned in the constructor.
        }
        var privateVarWithPrivateModulePropertyTypes: privateModule.publicClass;
        declare var privateAmbientVarWithPrivateModulePropertyTypes: privateModule.publicClass;
    }

    export interface publicInterfaceWithPrivatePropertyTypes {
        myProperty: privateClass;  // Error
    }

    export interface publicInterfaceWithPublicPropertyTypes {
        myProperty: publicClass;
    }

    interface privateInterfaceWithPrivatePropertyTypes {
        myProperty: privateClass;
    }

    interface privateInterfaceWithPublicPropertyTypes {
        myProperty: publicClass;
    }

    export class publicClassWithWithPrivatePropertyTypes {
        static myPublicStaticProperty: privateClass; // Error
        private static myPrivateStaticProperty: privateClass;
        myPublicProperty: privateClass; // Error
        //~^ ERROR: Property 'myPublicProperty' has no initializer and is not definitely assigned in the constructor.
        private myPrivateProperty: privateClass;
        //~^ ERROR: Property 'myPrivateProperty' has no initializer and is not definitely assigned in the constructor.
    }

    export class publicClassWithWithPublicPropertyTypes {
        static myPublicStaticProperty: publicClass;
        private static myPrivateStaticProperty: publicClass;
        myPublicProperty: publicClass;
        //~^ ERROR: Property 'myPublicProperty' has no initializer and is not definitely assigned in the constructor.
        private myPrivateProperty: publicClass;
        //~^ ERROR: Property 'myPrivateProperty' has no initializer and is not definitely assigned in the constructor.
    }

    class privateClassWithWithPrivatePropertyTypes {
        static myPublicStaticProperty: privateClass;
        private static myPrivateStaticProperty: privateClass;
        myPublicProperty: privateClass;
        //~^ ERROR: Property 'myPublicProperty' has no initializer and is not definitely assigned in the constructor.
        private myPrivateProperty: privateClass;
        //~^ ERROR: Property 'myPrivateProperty' has no initializer and is not definitely assigned in the constructor.
    }

    class privateClassWithWithPublicPropertyTypes {
        static myPublicStaticProperty: publicClass;
        private static myPrivateStaticProperty: publicClass;
        myPublicProperty: publicClass;
        //~^ ERROR: Property 'myPublicProperty' has no initializer and is not definitely assigned in the constructor.
        private myPrivateProperty: publicClass;
        //~^ ERROR: Property 'myPrivateProperty' has no initializer and is not definitely assigned in the constructor.
    }

    export var publicVarWithPrivatePropertyTypes: privateClass; // Error
    export var publicVarWithPublicPropertyTypes: publicClass;
    var privateVarWithPrivatePropertyTypes: privateClass;
    var privateVarWithPublicPropertyTypes: publicClass;

    export declare var publicAmbientVarWithPrivatePropertyTypes: privateClass; // Error
    export declare var publicAmbientVarWithPublicPropertyTypes: publicClass;
    declare var privateAmbientVarWithPrivatePropertyTypes: privateClass;
    declare var privateAmbientVarWithPublicPropertyTypes: publicClass;

    export interface publicInterfaceWithPrivateModulePropertyTypes {
        myProperty: privateModule.publicClass; // Error
    }
    export class publicClassWithPrivateModulePropertyTypes {
        static myPublicStaticProperty: privateModule.publicClass; // Error
        myPublicProperty: privateModule.publicClass; // Error
        //~^ ERROR: Property 'myPublicProperty' has no initializer and is not definitely assigned in the constructor.
    }
    export var publicVarWithPrivateModulePropertyTypes: privateModule.publicClass; // Error
    export declare var publicAmbientVarWithPrivateModulePropertyTypes: privateModule.publicClass; // Error

    interface privateInterfaceWithPrivateModulePropertyTypes {
        myProperty: privateModule.publicClass;
    }
    class privateClassWithPrivateModulePropertyTypes {
        static myPublicStaticProperty: privateModule.publicClass;
        myPublicProperty: privateModule.publicClass;
        //~^ ERROR: Property 'myPublicProperty' has no initializer and is not definitely assigned in the constructor.
    }
    var privateVarWithPrivateModulePropertyTypes: privateModule.publicClass;
    declare var privateAmbientVarWithPrivateModulePropertyTypes: privateModule.publicClass;
}