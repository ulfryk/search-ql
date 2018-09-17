import { Map } from 'immutable';
import { Maybe, None, Some } from 'monet';

import { Match, NodeEvaluation, ValueType } from '../../common/model';
import { BinaryOperatorRuntime } from '../../common/runtimes';

const getLabel = (selector: string, values: Map<string, string>): Maybe<string> =>
  values.has(selector) ? Some(selector) : None();

const getMatched = (matches: () => Maybe<Map<string, Match>>) =>
  (label: string) => matches()
    .map(someMatches => someMatches
      .filter((__, key) => key === label)
      .toMap());

export const notLike: BinaryOperatorRuntime<string, string, boolean> =
  (values, node) => (left, right) => {
    if (left.type === ValueType.Text && right.type === ValueType.Phrase) {
      return NodeEvaluation.ofPhrase(values, node)(
        getLabel(left.value, values)
          .flatMap(getMatched(right.matches))
          .orElse(Some(Map<string, Match>()))
          .filter(matched => matched.isEmpty()));
    }

    const value = ValueType.Text === left.type ?
      !left.value.includes(right.value) :
      left.value !== right.value;

    return NodeEvaluation.ofBoolean(values, node)(value);
  };
