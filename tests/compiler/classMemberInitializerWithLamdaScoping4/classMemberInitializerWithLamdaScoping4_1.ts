declare var console: {
    log(msg?: any): void;
};
export class Test1 {
    constructor(private field1: string) {
    }
    messageHandler = () => {
        console.log(field1); // Should be error that couldnt find symbol field1
        //~^ ERROR: Cannot find name 'field1'.
    };
}