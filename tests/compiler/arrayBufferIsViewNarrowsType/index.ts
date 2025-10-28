// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/arrayBufferIsViewNarrowsType.ts`, Apache-2.0 License

var obj: Object;
if (ArrayBuffer.isView(obj)) {
    // isView should be a guard that narrows type to ArrayBufferView.
    var ab: ArrayBufferView = obj;
}