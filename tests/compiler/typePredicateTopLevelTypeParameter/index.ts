// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typePredicateTopLevelTypeParameter.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict


function getPermissions(user: string) {
    if (user === 'Jack') return 'admin';
    return undefined;
}

const admins = ['Mike', 'Joe'].map(e => getPermissions(e));

function isDefined<T>(a: T | undefined): a is T {
    return a !== undefined;
}

const foundAdmins = admins.filter(isDefined);  // "admin"[]
