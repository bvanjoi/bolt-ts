// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/acceptSymbolAsWeakType.ts`, Apache-2.0 License
//@compiler-options: target=esnext
//@compiler-options: lib=[esnext]
var s = Symbol('s');
var ws = new WeakSet([s]);
ws.add(s);
ws.has(s);
ws.delete(s);
var wm = new WeakMap([[s, false]]);
wm.set(s, true);
wm.has(s);
wm.get(s);
wm.delete(s);
var wr = new WeakRef(s);
wr.deref();
var f = new FinalizationRegistry(() => {});
f.register(s, null);
f.unregister(s);