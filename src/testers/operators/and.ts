import { Match, NodeEvaluation } from '../../common/model';
import { BinaryOperatorRuntime } from '../../common/runtimes';

export const and: BinaryOperatorRuntime<boolean, boolean, boolean> =
  (values, node) => (left, right) => {
    const value = left.value && right.value;
    const matches = () => left.matches().flatMap(leftMatches => right.matches()
      .map(rightMatches => leftMatches.entrySeq()
        .concat(rightMatches.entrySeq())
        .groupBy(([label]) => label)
        .map(group => group
          .map(([__, match]) => match)
          .reduce((acc: Match, match: Match) => acc.and(match)))
        .toMap()));

    return NodeEvaluation.fromLogic(left.type, right.type)(values, node)(value, matches);
  };
