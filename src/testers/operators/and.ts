import { Match, NodeEvaluation } from '../../common/model';
import { BinaryOperatorRuntime } from '../../common/runtimes';

export const and: BinaryOperatorRuntime<boolean, boolean, boolean> =
  (values, node) => (left, right) =>
    NodeEvaluation.ofBoolean(values, node)(
      left.value && right.value,
      () => left.matches().flatMap(leftMatches => right.matches()
        .map(rightMatches => leftMatches.entrySeq()
          .concat(rightMatches.entrySeq())
          .groupBy(([label]) => label)
          .map(group => group
            .map(([__, match]) => match)
            .reduce((acc: Match, match: Match) => acc.and(match)))
          .toMap())));
