// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/divergentAccessorsVisibility1.ts`, Apache-2.0 License

//@compiler-options: strict
//@compiler-options: target=esnext

class Base {
    get PublicPublic() { return 0; }
    set PublicPublic(v) { return; }

    get PublicProtected() { return 0; }
    protected set PublicProtected(v) { return; }

    get PublicPrivate() { return 0; }
    private set PublicPrivate(v) { return; }

    protected get ProtectedPublic() { return 0; }
    //~^ ERROR: A get accessor must be at least as accessible as the setter.
    set ProtectedPublic(v) { return; }

    protected get ProtectedProtected() { return 0; }
    protected set ProtectedProtected(v) { return; }

    protected get ProtectedPrivate() { return 0; }
    private set ProtectedPrivate(v) { return; }

    private get PrivatePublic() { return 0; }
    //~^ ERROR: A get accessor must be at least as accessible as the setter.
    set PrivatePublic(v) { return; }

    private get PrivateProtected() { return 0; }
    //~^ ERROR: A get accessor must be at least as accessible as the setter.
    protected set PrivateProtected(v) { return; }

    private get PrivatePrivate() { return 0; }
    private set PrivatePrivate(v) { return; }

    test() {
        this.PublicPublic = 0;
        this.PublicProtected = 0;
        this.PublicPrivate = 0;
        this.ProtectedPublic = 0;
        this.ProtectedProtected = 0;
        this.ProtectedPrivate = 0;
        this.PrivatePublic = 0;
        this.PrivateProtected = 0;
        this.PrivatePrivate = 0;

        void this.PublicPublic;
        void this.PublicProtected;
        void this.PublicPrivate;
        void this.ProtectedPublic;
        void this.ProtectedProtected;
        void this.ProtectedPrivate;
        void this.PrivatePublic;
        void this.PrivateProtected;
        void this.PrivatePrivate;

        this.PublicPublic += 0;
        this.PublicProtected += 0;
        this.PublicPrivate += 0;
        this.ProtectedPublic += 0;
        this.ProtectedProtected += 0;
        this.ProtectedPrivate += 0;
        this.PrivatePublic += 0;
        this.PrivateProtected += 0;
        this.PrivatePrivate += 0;
    }
}

class Derived extends Base {
    test2() {
        this.PublicPublic = 0;
        this.PublicProtected = 0;
        this.PublicPrivate = 0;
        //~^ ERROR: Property 'PublicPrivate' is private and only accessible within class 'Base'.
        this.ProtectedPublic = 0;
        this.ProtectedProtected = 0;
        this.ProtectedPrivate = 0;
        //~^ ERROR: Property 'ProtectedPrivate' is private and only accessible within class 'Base'.
        this.PrivatePublic = 0;
        this.PrivateProtected = 0;
        this.PrivatePrivate = 0;
        //~^ ERROR: Property 'PrivatePrivate' is private and only accessible within class 'Base'.

        void this.PublicPublic;
        void this.PublicProtected;
        void this.PublicPrivate;
        void this.ProtectedPublic;
        void this.ProtectedProtected;
        void this.ProtectedPrivate;
        void this.PrivatePublic;
        //~^ ERROR: Property 'PrivatePublic' is private and only accessible within class 'Base'.
        void this.PrivateProtected;
        //~^ ERROR: Property 'PrivateProtected' is private and only accessible within class 'Base'.
        void this.PrivatePrivate;
        //~^ ERROR: Property 'PrivatePrivate' is private and only accessible within class 'Base'.

        this.PublicPublic += 0;
        this.PublicProtected += 0;
        this.PublicPrivate += 0;
        //~^ ERROR: Property 'PublicPrivate' is private and only accessible within class 'Base'.
        this.ProtectedPublic += 0;
        this.ProtectedProtected += 0;
        this.ProtectedPrivate += 0;
        //~^ ERROR: Property 'ProtectedPrivate' is private and only accessible within class 'Base'.
        this.PrivatePublic += 0;
        this.PrivateProtected += 0;
        this.PrivatePrivate += 0;
        //~^ ERROR: Property 'PrivatePrivate' is private and only accessible within class 'Base'.
    }
}

declare const base: Base, deriv: Derived;
function fn() {
    base.PublicPublic = 0;
    base.PublicProtected = 0;
    //~^ ERROR: Property 'PublicProtected' is protected and only accessible within class 'Base' and its subclasses.
    base.PublicPrivate = 0;
    //~^ ERROR: Property 'PublicPrivate' is private and only accessible within class 'Base'.
    base.ProtectedPublic = 0;
    base.ProtectedProtected = 0;
    //~^ ERROR: Property 'ProtectedProtected' is protected and only accessible within class 'Base' and its subclasses.
    base.ProtectedPrivate = 0;
    //~^ ERROR: Property 'ProtectedPrivate' is private and only accessible within class 'Base'.
    base.PrivatePublic = 0;
    base.PrivateProtected = 0;
    //~^ ERROR: Property 'PrivateProtected' is protected and only accessible within class 'Base' and its subclasses.
    base.PrivatePrivate = 0;
    //~^ ERROR: Property 'PrivatePrivate' is private and only accessible within class 'Base'.

    void base.PublicPublic;
    void base.PublicProtected;
    void base.PublicPrivate;
    void base.ProtectedPublic;
    //~^ ERROR: Property 'ProtectedPublic' is protected and only accessible within class 'Base' and its subclasses.
    void base.ProtectedProtected;
    //~^ ERROR: Property 'ProtectedProtected' is protected and only accessible within class 'Base' and its subclasses.
    void base.ProtectedPrivate;
    //~^ ERROR: Property 'ProtectedPrivate' is protected and only accessible within class 'Base' and its subclasses.
    void base.PrivatePublic;
    //~^ ERROR: Property 'PrivatePublic' is private and only accessible within class 'Base'.
    void base.PrivateProtected;
    //~^ ERROR: Property 'PrivateProtected' is private and only accessible within class 'Base'.
    void base.PrivatePrivate;
    //~^ ERROR: Property 'PrivatePrivate' is private and only accessible within class 'Base'.

    base.PublicPublic += 0;
    base.PublicProtected += 0;
    //~^ ERROR: Property 'PublicProtected' is protected and only accessible within class 'Base' and its subclasses.
    base.PublicPrivate += 0;
    //~^ ERROR: Property 'PublicPrivate' is private and only accessible within class 'Base'.
    base.ProtectedPublic += 0;
    base.ProtectedProtected += 0;
    //~^ ERROR: Property 'ProtectedProtected' is protected and only accessible within class 'Base' and its subclasses.
    base.ProtectedPrivate += 0;
    //~^ ERROR: Property 'ProtectedPrivate' is private and only accessible within class 'Base'.
    base.PrivatePublic += 0;
    base.PrivateProtected += 0;
    //~^ ERROR: Property 'PrivateProtected' is protected and only accessible within class 'Base' and its subclasses.
    base.PrivatePrivate += 0;
    //~^ ERROR: Property 'PrivatePrivate' is private and only accessible within class 'Base'.

    deriv.PublicPublic = 0;
    deriv.PublicProtected = 0;
    //~^ ERROR: Property 'PublicProtected' is protected and only accessible within class 'Base' and its subclasses.
    deriv.PublicPrivate = 0;
    //~^ ERROR: Property 'PublicPrivate' is private and only accessible within class 'Base'.
    deriv.ProtectedPublic = 0;
    deriv.ProtectedProtected = 0;
    //~^ ERROR: Property 'ProtectedProtected' is protected and only accessible within class 'Base' and its subclasses.
    deriv.ProtectedPrivate = 0;
    //~^ ERROR: Property 'ProtectedPrivate' is private and only accessible within class 'Base'.
    deriv.PrivatePublic = 0;
    deriv.PrivateProtected = 0;
    //~^ ERROR: Property 'PrivateProtected' is protected and only accessible within class 'Base' and its subclasses.
    deriv.PrivatePrivate = 0;
    //~^ ERROR: Property 'PrivatePrivate' is private and only accessible within class 'Base'.

    void deriv.PublicPublic;
    void deriv.PublicProtected;
    void deriv.PublicPrivate;
    void deriv.ProtectedPublic;
    //~^ ERROR: Property 'ProtectedPublic' is protected and only accessible within class 'Base' and its subclasses.
    void deriv.ProtectedProtected;
    //~^ ERROR: Property 'ProtectedProtected' is protected and only accessible within class 'Base' and its subclasses.
    void deriv.ProtectedPrivate;
    //~^ ERROR: Property 'ProtectedPrivate' is protected and only accessible within class 'Base' and its subclasses.
    void deriv.PrivatePublic;
    //~^ ERROR: Property 'PrivatePublic' is private and only accessible within class 'Base'.
    void deriv.PrivateProtected;
    //~^ ERROR: Property 'PrivateProtected' is private and only accessible within class 'Base'.
    void deriv.PrivatePrivate;
    //~^ ERROR: Property 'PrivatePrivate' is private and only accessible within class 'Base'.

    deriv.PublicPublic += 0;
    deriv.PublicProtected += 0;
    //~^ ERROR: Property 'PublicProtected' is protected and only accessible within class 'Base' and its subclasses.
    deriv.PublicPrivate += 0;
    //~^ ERROR: Property 'PublicPrivate' is private and only accessible within class 'Base'.
    deriv.ProtectedPublic += 0;
    deriv.ProtectedProtected += 0;
    //~^ ERROR: Property 'ProtectedProtected' is protected and only accessible within class 'Base' and its subclasses.
    deriv.ProtectedPrivate += 0;
    //~^ ERROR: Property 'ProtectedPrivate' is private and only accessible within class 'Base'.
    deriv.PrivatePublic += 0;
    deriv.PrivateProtected += 0;
    //~^ ERROR: Property 'PrivateProtected' is protected and only accessible within class 'Base' and its subclasses.
    deriv.PrivatePrivate += 0;
    //~^ ERROR: Property 'PrivatePrivate' is private and only accessible within class 'Base'.
}
