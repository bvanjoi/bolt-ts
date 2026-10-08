// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/accessorInferredReturnTypeErrorInReturnStatement.ts`, Apache-2.0 License

//@compiler-options: module=commonjs
//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration

export var basePrototype = {
  get primaryPath() {
    //~^ ERROR: 'primaryPath' implicitly has return type 'any' because it does not have a return type annotation and is referenced directly or indirectly in one of its return expressions.
    var _this = this;
    return _this.collection.schema.primaryPath;
    //~^ ERROR: Property 'collection' does not exist on type '{ primaryPath: error; }'.
  },  
};
