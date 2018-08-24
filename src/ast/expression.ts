import { ISetoid } from '@samwise-tech/core';

export abstract class Expression implements ISetoid {
  public abstract readonly value: any;
  public abstract equals(other: Expression): boolean;
  public abstract toString(): string;
}
