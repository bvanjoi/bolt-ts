// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/complicatedIndexedAccessKeyofReliesOnKeyofNeverUpperBound.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface TextChannel {
    id: string;
    type: 'text';
    phoneNumber: string;
}

interface EmailChannel {
    id: string;
    type: 'email';
    addres: string;
}

type Channel = TextChannel | EmailChannel;

export type ChannelType = Channel extends { type: infer R } ? R : never;

type Omit<T, K extends keyof T> = Pick<
    T,
    ({ [P in keyof T]: P } & { [P in K]: never } & { [x: string]: never })[keyof T]
>;

type ChannelOfType<T extends ChannelType, A = Channel> = A extends { type: T }
    ? A
    : never;


export type NewChannel<T extends Channel> = Pick<T, 'type'> &
    Partial<Omit<T, 'type' | 'id'>> & { localChannelId: string };


export function makeNewChannel<T extends ChannelType>(type: T): NewChannel<ChannelOfType<T>> {
    const localChannelId = `blahblahblah`;
    return { type, localChannelId };
    //~^ ERROR: Type '{ type: T; localChannelId: string; }' is not assignable to type 'NewChannel<ChannelOfType<T, TextChannel> | ChannelOfType<T, EmailChannel>>'.
}

const newTextChannel = makeNewChannel('text');
// This should work
newTextChannel.phoneNumber = '613-555-1234';

const newTextChannel2 : NewChannel<TextChannel> = makeNewChannel('text');
// Compare with this, which ofc works.
newTextChannel2.phoneNumber = '613-555-1234';
