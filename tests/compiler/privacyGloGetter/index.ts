// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/privacyGloGetter.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

namespace m1 {
    export class C1_public {
        private f1() {
        }
    }

    class C2_private {
    }

    export class C3_public {
        private get p1_private() {
            return new C1_public();
        }

        private set p1_private(m1_c3_p1_arg: C1_public) {
        }

        private get p2_private() {
            return new C1_public();
        }

        private set p2_private(m1_c3_p2_arg: C1_public) {
        }

        private get p3_private() {
            return new C2_private();
        }

        private set p3_private(m1_c3_p3_arg: C2_private) {
        }

        public get p4_public(): C2_private { // error
            return new C2_private(); //error
        }

        public set p4_public(m1_c3_p4_arg: C2_private) { // error
        }
    }

    class C4_private {
        private get p1_private() {
            return new C1_public();
        }

        private set p1_private(m1_c3_p1_arg: C1_public) {
        }

        private get p2_private() {
            return new C1_public();
        }

        private set p2_private(m1_c3_p2_arg: C1_public) {
        }

        private get p3_private() {
            return new C2_private();
        }

        private set p3_private(m1_c3_p3_arg: C2_private) {
        }

        public get p4_public(): C2_private {
            return new C2_private();
        }

        public set p4_public(m1_c3_p4_arg: C2_private) {
        }
    }
}

class C6_public {
}

class C7_public {
    private get p1_private() {
        return new C6_public();
    }

    private set p1_private(m1_c3_p1_arg: C6_public) {
    }

    private get p2_private() {
        return new C6_public();
    }

    private set p2_private(m1_c3_p2_arg: C6_public) {
    }
}