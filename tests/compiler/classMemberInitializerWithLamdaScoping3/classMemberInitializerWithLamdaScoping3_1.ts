declare var console: {
    log(msg?: any): void;
};
export class Test1 {
    constructor(private field1: string) {
    }
    messageHandler = () => {
        console.log(field1); // But this should be error as the field1 will resolve to var field1 
                             // but since this code would be generated inside constructor, in generated js
                             // it would resolve to private field1 and thats not what user intended here. 
        //~^^^ERROR: Initializer of instance member variable 'messageHandler' cannot reference identifier 'field1' declared in the constructor.
    };
}