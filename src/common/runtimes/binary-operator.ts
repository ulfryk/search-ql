import { Map } from 'immutable';
import { Maybe } from 'monet';

import { Match } from '../model';

export type BinaryOperatorRuntime = (
  a: Maybe<Map<string, Match>>,
  b: () => Maybe<Map<string, Match>>,
) => Maybe<Map<string, Match>>;
