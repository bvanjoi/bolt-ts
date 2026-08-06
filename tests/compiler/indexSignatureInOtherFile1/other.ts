interface Array1<T> {
  length: number;
  [n: number]: T;
}

interface ArrayConstructor1 {
  new(arrayLength?: number): Array1<any>;
}

declare var Array1: ArrayConstructor1;