// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/partialTypeNarrowedToByTypeGuard.ts`, Apache-2.0 License

type Obj = {} | undefined;

type User = {
    email: string;
    name: string;
};

type PartialUser = Partial<User>;
// let b: PartialUser['name'] = 1;

// // type PartialUser = {
// //   email?: string;
// //   name?: string;
// // };

function isUser(obj: Obj): obj is PartialUser {
    return true;
}

// function getUserName(obj: Obj) {
//     if (isUser(obj)) {
//         return obj.name;
//     }

//     return '';
// }

// let s: string = getUserName({})
