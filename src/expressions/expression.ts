import { ISetoid } from '@samwise-tech/core/model/fantasy-land/setoid';
import { Map } from 'immutable';
import { Maybe } from 'monet';

import { Match } from '../match';

export abstract class Expression implements ISetoid {
  public abstract readonly value: any;
  public abstract equals(other: Expression): boolean;
  public abstract toString(): string;
  public abstract test(values: Map<string, string>): Maybe<Map<string, Match>>;
}
