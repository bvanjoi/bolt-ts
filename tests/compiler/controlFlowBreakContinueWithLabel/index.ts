// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/controlFlowBreakContinueWithLabel.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

enum User { A, B }

let user: User = User.A

label: while (true) {
    switch (user) {
        case User.A:
            user = User.B;
            user = 42;
            //~^ ERROR: Type 'number' is not assignable to type 'User.A | User.B'.
            continue label;
        case User.B:
            break label;
    }
}
user;
