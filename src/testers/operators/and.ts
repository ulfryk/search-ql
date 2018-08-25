import { Map } from 'immutable';
import { Maybe } from 'monet';

import { Match } from '../../match';

export const and = (a: Maybe<Map<string, Match>>, b: () => Maybe<Map<string, Match>>) =>
  a.flatMap(someA => b()
    .map(someB => someA.entrySeq()
      .concat(someB.entrySeq())
      .groupBy(([label]) => label)
      .map(group => group
        .map(([__, match]) => match)
        .reduce((acc: Match, match: Match) => acc.and(match)))
      .toMap()));
