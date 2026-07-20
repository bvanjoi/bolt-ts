type Hash = string & {
  __hash: true;
};
declare function getHashLength(hash: Hash): number;
declare function getHashLength2<T extends {
  __tag__: unknown;
}>(hash: string & T): number;
