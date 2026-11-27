export var test1: <T>(schema: { }) => void;
export function test2<T = Record<string, never>>(schema: {
[K in keyof Required<T>]: T[K]
}): void;

