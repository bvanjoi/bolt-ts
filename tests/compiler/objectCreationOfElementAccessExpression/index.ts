// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectCreationOfElementAccessExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Food {
    private amount: number;
    constructor(public name: string) {
        this.amount = 100;
    }
    public eat(amountToEat: number): boolean {
        this.amount -= amountToEat;
        if (this.amount <= 0) {
            this.amount = 0;
            return false;
        }
        else {
            return true;
        }
    }
}
class MonsterFood extends Food {
    constructor(name: string, public flavor: string) {
        super(name);
    }
}
class IceCream extends MonsterFood {
    private isDairyFree: boolean;
    //~^ ERROR: Property 'isDairyFree' has no initializer and is not definitely assigned in the constructor.
    constructor(public flavor: string) {
        super("Ice Cream", flavor);
    }
}
class Cookie extends MonsterFood {
    constructor(public flavor: string, public isGlutenFree: boolean) {
        super("Cookie", flavor);
    }
}
class PetFood extends Food {
    constructor(name: string, public whereToBuy: number) {
        super(name);
    }
}
class ExpensiveOrganicDogFood extends PetFood {
    constructor(public whereToBuy: number) {
        super("Origen", whereToBuy);
    }
}
class ExpensiveOrganicCatFood extends PetFood {
    constructor(public whereToBuy: number, public containsFish: boolean) {
        super("Nature's Logic", whereToBuy);
    }
}
class Slug {
    // This is NOT a food!!!
}

// ElementAccessExpressions can only contain one expression.  There should be a parse error here.
var foods = new PetFood[new IceCream('Mint chocolate chip') , Cookie('Chocolate chip', false) , new Cookie('Peanut butter', true)];
//~^ ERROR: Value of type 'typeof Cookie' is not callable.
//~| ERROR: Type 'Cookie' cannot be used as an index type.
var foods2: MonsterFood[] = new PetFood[new IceCream('Mint chocolate chip') , Cookie('Chocolate chip', false) , new Cookie('Peanut butter', true)];
//~^ ERROR: Value of type 'typeof Cookie' is not callable.
//~| ERROR: Type 'Cookie' cannot be used as an index type.