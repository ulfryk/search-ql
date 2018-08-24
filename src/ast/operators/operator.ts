import { ISetoid } from '@samwise-tech/core';

export abstract class Operator implements ISetoid {

  public is<C extends typeof Operator>(ctor: C) {
    return this instanceof ctor;
  }

  public equals(other: Operator): boolean {
    return this === other || other.is(this.constructor as typeof Operator);
  }

  public abstract toString(): string;
}
