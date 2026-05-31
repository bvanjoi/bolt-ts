// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericIndexedAccessMethodIntersectionCanBeAccessed.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type ExtendedService<T> = {
    [K in keyof T]: T[K] & {
        __$daemonMode?: string;
        __$action?: string;
    };
};

type Service<T> = {
    [K in keyof T]: T[K] & {id?: string};
};

export const createService = <T>(
    ServiceCtr: ExtendedService<T> & Service<T>
) => {
    Object.keys(ServiceCtr).forEach(key => {
        const method = (ServiceCtr)[key as keyof T];
        const {__$daemonMode, __$action, id} = method;
    })
}
