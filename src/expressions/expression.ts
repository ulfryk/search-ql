import { ISetoid } from '@samwise-tech/core';
import { Map } from 'immutable';
import { Maybe } from 'monet';

import { Match } from '../match';
import { SyntaxConfig } from '../syntax-config';

export abstract class Expression implements ISetoid {
  public abstract readonly value: any;
  public abstract equals(other: Expression): boolean;
  public abstract toString(): string;
  public abstract test(
    values: Map<string, string>,
    config: SyntaxConfig,
  ): Maybe<Map<string, Match>>;
}
