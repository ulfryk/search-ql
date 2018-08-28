import { BinaryOperatorRuntime } from './binary-operator-runtime';

export const like: BinaryOperatorRuntime = (labels, getExpression) =>
  labels
    .map(someLabels => someLabels.keySeq().toSet())
    .flatMap(someLabels => getExpression()
      .map(matches => matches
        .filter((_match, key) => someLabels.has(key))
        .toMap())
      .filter(matched => !matched.isEmpty()));
