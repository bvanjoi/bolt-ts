// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classMemberInitializerWithLamdaScoping.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es5]

declare var console: {
    log(msg?: any): void;
};
class Test {
    constructor(private field: string) {
    }
    messageHandler = () => {
        var field = this.field;
        console.log(field); // Using field here shouldnt be error
    };
    static field: number;
    static staticMessageHandler = () => {
        var field = Test.field;
        console.log(field); // Using field here shouldnt be error
    };
}

var field1: string;
class Test1 {
    constructor(private field1: string) {
    }
    messageHandler = () => {
        console.log(field1); // But this should be error as the field1 will resolve to var field1 
                             // but since this code would be generated inside constructor, in generated js
                             // it would resolve to private field1 and thats not what user intended here. 
        //~^^^ERROR: Initializer of instance member variable 'messageHandler' cannot reference identifier 'field1' declared in the constructor.
    };
    static staticMessageHandler = () => {
        console.log(field1); // This shouldnt be error as its a static property
    };
}