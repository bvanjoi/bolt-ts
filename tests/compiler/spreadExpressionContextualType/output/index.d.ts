interface Orange {
  name: string;
}
interface Apple {
  name: string;
}
declare function test<T extends Apple | Orange>(item: T): T;
declare function test2<T extends Apple | Orange>(item: T): T;
