// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/abstractClassUnionInstantiation.ts`, Apache-2.0 License

class ConcreteA {}
class ConcreteB {}
abstract class AbstractA { a: string; }
abstract class AbstractB { b: string; }

type Abstracts = typeof AbstractA | typeof AbstractB;
type Concretes = typeof ConcreteA | typeof ConcreteB;
type ConcretesOrAbstracts = Concretes | Abstracts;

declare const cls1: ConcretesOrAbstracts;
declare const cls2: Abstracts;
declare const cls3: Concretes;

new cls1();
//~^ ERROR: Cannot create an instance of an abstract class.
new cls2();
//~^ ERROR: Cannot create an instance of an abstract class.
new cls3();

[ConcreteA, AbstractA, AbstractB].map(cls => new cls()); 
//~^ ERROR: Cannot create an instance of an abstract class.
[AbstractA, AbstractB, ConcreteA].map(cls => new cls()); 
//~^ ERROR: Cannot create an instance of an abstract class.
[ConcreteA, ConcreteB].map(cls => new cls());
[AbstractA, AbstractB].map(cls => new cls());
//~^ ERROR: Cannot create an instance of an abstract class.
