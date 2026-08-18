// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructuringWithGenericParameter.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class GenericClass<T> {
    payload: T;
    //~^ ERROR: Property 'payload' has no initializer and is not definitely assigned in the constructor.
}

var genericObject = new GenericClass<{ greeting: string }>();

function genericFunction<T>(object: GenericClass<T>, callback: (payload: T) => void) {
    callback(object.payload);
}

genericFunction(genericObject, ({greeting}) => {
    var s = greeting.toLocaleLowerCase();  // Greeting should be of type string
});
