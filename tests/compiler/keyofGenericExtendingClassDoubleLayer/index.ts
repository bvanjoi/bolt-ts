// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/keyofGenericExtendingClassDoubleLayer.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Model<Attributes = any> {
    public createdAt: Date;
    //~^ ERROR: Property 'createdAt' has no initializer and is not definitely assigned in the constructor.
}

type ModelAttributes<T> = Omit<T, keyof Model>;

class AutoModel<T> extends Model<ModelAttributes<T>> {}

class PersonModel extends AutoModel<PersonModel> {
    public age: number;
    //~^ ERROR: Property 'age' has no initializer and is not definitely assigned in the constructor.

    toJson() {
        let x: keyof this = 'createdAt';
        let y: keyof this = 'age';
        let z: keyof this = 'toJson';
    }
}
