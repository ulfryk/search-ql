import { Match } from '../../match';
import { BinaryOperatorRuntime } from './binary-operator-runtime';

export const and: BinaryOperatorRuntime = (leftSide, getRightSide) =>
  leftSide.flatMap(left => getRightSide()
    .map(right => left.entrySeq()
      .concat(right.entrySeq())
      .groupBy(([label]) => label)
      .map(group => group
        .map(([__, match]) => match)
        .reduce((acc: Match, match: Match) => acc.and(match)))
      .toMap()));
