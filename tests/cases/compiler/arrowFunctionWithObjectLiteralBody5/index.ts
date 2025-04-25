// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/arrowFunctionWithObjectLiteralBody5.ts`, Apache-2.0 License

var a = () => <Error>{ name: "foo", message: "bar" };      

var b = () => (<Error>{ name: "foo", message: "bar" });    

var c = () => ({ name: "foo", message: "bar" });           

var d = () => ((<Error>({ name: "foo", message: "bar" })));


<Error>{ name: "foo", message: "bar" };

const f = <Error>{ name: "foo", message: "bar" };