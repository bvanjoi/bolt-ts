// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/dissallowSymbolAsWeakType.ts`, Apache-2.0 License

//@compiler-options: lib=[es2022]
//@compiler-options: target=es2022

const s: symbol = Symbol('s');

const ws = new WeakSet([s]);
//~^ ERROR: No overload matches this call.
ws.add(s);
//~^ ERROR: Argument of type 'symbol' is not assignable to parameter of type 'object'.
ws.has(s);
//~^ ERROR: Argument of type 'symbol' is not assignable to parameter of type 'object'.
ws.delete(s);
//~^ ERROR: Argument of type 'symbol' is not assignable to parameter of type 'object'.

const wm = new WeakMap([[s, false]]);
//~^ ERROR: No overload matches this call.
wm.set(s, true);
//~^ ERROR: Argument of type 'symbol' is not assignable to parameter of type 'object'.
wm.has(s);
//~^ ERROR: Argument of type 'symbol' is not assignable to parameter of type 'object'.
wm.get(s);
//~^ ERROR: Argument of type 'symbol' is not assignable to parameter of type 'object'.
wm.delete(s);
//~^ ERROR: Argument of type 'symbol' is not assignable to parameter of type 'object'.

const wr = new WeakRef(s);
//~^ ERROR: Argument of type 'symbol' is not assignable to parameter of type 'object'.
wr.deref();

const f = new FinalizationRegistry(() => {});
f.register(s, null);
//~^ ERROR: Argument of type 'symbol' is not assignable to parameter of type 'object'.
f.unregister(s);
//~^ ERROR: Argument of type 'symbol' is not assignable to parameter of type 'object'.
