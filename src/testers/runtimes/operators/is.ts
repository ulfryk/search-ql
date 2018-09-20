import { Map } from 'immutable';
import { Maybe, None, Some } from 'monet';

import { Match, ValueType } from '../../../index';

import { BinaryOperatorRuntime, NodeEvaluation } from '../../model';

const getLabel = (selector: string, values: Map<string, string>): Maybe<string> =>
  values.has(selector) ? Some(selector) : None();

const getMatched = (matches: () => Maybe<Map<string, Match>>) =>
  (label: string): Maybe<Map<string, Match>> => matches()
    .map(someMatches => someMatches
      .filter((match, key) => key === label && match.isFullMatch())
      .toMap());

export const is: BinaryOperatorRuntime<string, string, boolean> =
  (values, node) => (left, right) => {
    if (left.type === ValueType.Text && right.type === ValueType.Phrase) {
      return NodeEvaluation.ofPhrase(values, node)(
        getLabel(left.value, values)
          .flatMap(getMatched(right.matches))
          .filter(matched => !matched.isEmpty()));
    }

    return NodeEvaluation.ofBoolean(values, node)(left.value === right.value);
  };
