interface MyFunction { msg: string; }

export const MyFunction = ({ msg }: MyFunction) => console.log(`Got message "${msg}"`);
export default MyFunction;