import { ISetoid } from '@samwise-tech/core/model';
import { Map } from 'immutable';

export abstract class Expression implements ISetoid {
  public abstract readonly value: any;
  public abstract equals(other: Expression): boolean;
  public abstract toString(): string;
  public abstract test(values: Map<string, string>): boolean;
}
