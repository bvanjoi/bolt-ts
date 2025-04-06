type O = {
  b?: boolean;	
}
type B<T extends boolean> = T;
type A<Options extends Required<O>> = B<Options['b']>;