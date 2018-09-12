import { Map } from 'immutable';
import { Maybe, None, Some } from 'monet';

import { Match, NodeEvaluation } from '../../common/model';
import { BinaryOperatorRuntime } from '../../common/runtimes';

const getLabel = (selector: string, values: Map<string, string>): Maybe<string> =>
  values.has(selector) ? Some(selector) : None();

const getMatched = (matches: () => Maybe<Map<string, Match>>) =>
  (label: string) => matches()
    .map(someMatches => someMatches
      .filter((__, key) => key === label)
      .toMap());

export const like: BinaryOperatorRuntime<string, string, boolean> =
  (values, node) => (left, right) =>
    NodeEvaluation.ofPhrase(values, node)(
      getLabel(left.value, values)
        .flatMap(getMatched(right.matches))
        .filter(matched => !matched.isEmpty()));
