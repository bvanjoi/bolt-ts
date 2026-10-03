function isNonNullable<T>(obj: T): asserts obj is NonNullable<T> {
    if (obj === undefined || obj === null) {
        throw new Error("Must not be a nullable value");
    }
}

export {
    isNonNullable
};