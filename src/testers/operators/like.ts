import { Map } from 'immutable';
import { Some } from 'monet';

import { NodeEvaluation } from '../../common/model';
import { BinaryOperatorRuntime } from '../../common/runtimes';

const getLabels = (selector: string, values: Map<string, string>) =>
  Some(values.keySeq().toSet().filter(key => key === selector))
    .filter(keys => !keys.isEmpty());

export const like: BinaryOperatorRuntime<string, string, boolean> =
  (values, node) => (left, right) =>
    NodeEvaluation.ofPhrase(values, node)(getLabels(left.value, values)
      .flatMap(someLabels => right.matches()
        .map(matches => matches
          .filter((_match, key) => someLabels.has(key))
          .toMap())
        .filter(matched => !matched.isEmpty())));
