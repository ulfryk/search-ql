import { Maybe, Some } from 'monet';

import { Match } from '../../match';

export const or = (a: Maybe<Map<string, Match>>, b: () => Maybe<Map<string, Match>>) =>
  a.cata(b, Some);
