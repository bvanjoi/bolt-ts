// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/spreadTypeRemovesReadonly.ts`, Apache-2.0 License

interface ReadonlyData {
    readonly value: string;
}

const data: ReadonlyData = { value: 'foo' };
const clone = { ...data };
clone.value = 'bar';
