type T1 = {
  0?: string | undefined;
} extends {
  0?: string;
} ? true : false;
type T2 = [(string | undefined)] extends [string] ? true : false;
