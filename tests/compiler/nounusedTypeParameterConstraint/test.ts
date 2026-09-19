import { IEventSourcedEntity } from "./bar";
export type DomainEntityConstructor<TEntity extends IEventSourcedEntity> = { new(): TEntity; };