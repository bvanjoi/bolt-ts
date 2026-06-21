// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/acceptSymbolAsWeakType.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: lib=[esnext]
//@compiler-options: strict

const s: symbol = Symbol('s');

const ws = new WeakSet([s]);
ws.add(s);
ws.has(s);
ws.delete(s);

const wm = new WeakMap([[s, false]]);
wm.set(s, true);
wm.has(s);
wm.get(s);
wm.delete(s);

const wr = new WeakRef(s);
wr.deref();

const f = new FinalizationRegistry(() => {});
f.register(s, null);
f.unregister(s);