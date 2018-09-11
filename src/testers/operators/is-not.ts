import { Some } from 'monet';

import { PhraseExpression } from '../../ast';
import { Match, NodeEvaluation } from '../../common/model';
import { BinaryOperatorRuntime } from '../../common/runtimes';

export const isNot: BinaryOperatorRuntime<string, string, boolean> =
  (values, node) => (left, right) =>
    NodeEvaluation.ofPhrase(values, node)(
      Some(values
        .filter((value, key) =>
          key === left.value &&
            value.trim().toLowerCase() ===
              (right.node as PhraseExpression).preparedValue.toLowerCase())
        .map(Match.whole)
        .toMap())
      .filter(matches => matches.isEmpty()));
