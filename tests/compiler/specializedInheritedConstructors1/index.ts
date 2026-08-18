// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/specializedInheritedConstructors1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface ViewOptions<TModel> {
    model: TModel;
}

class View<TModel> {
    constructor(options: ViewOptions<TModel>) { }
    model: TModel;
    //~^ ERROR: Property 'model' has no initializer and is not definitely assigned in the constructor.
}

class Model { }
class MyView extends View<Model> { }

var m: ViewOptions<Model> = { model: new Model() };
var aView = new View({ model: new Model() }); 
var aView2 = new View(m); 
var myView = new MyView(m); // was error
