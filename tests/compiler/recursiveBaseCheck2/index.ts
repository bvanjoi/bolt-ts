// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveBaseCheck2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare namespace Box2D.Collision.Shapes {
    export class b2CircleShape extends b2Shape {
      //~^ ERROR: 'b2CircleShape' is referenced directly or indirectly in its own base expression.
    }
    export class b2Shape extends Box2D.Collision.Shapes.b2CircleShape {
      //~^ ERROR: 'b2Shape' is referenced directly or indirectly in its own base expression.
    }
}
declare namespace Box2D.Dynamics {
    export class b2ContactListener extends Box2D.Collision.Shapes.b2Shape {
    }
    export class b2FixtureDef extends Box2D.Dynamics.b2ContactListener {
    }
}