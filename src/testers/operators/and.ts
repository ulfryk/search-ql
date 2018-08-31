import { Match } from '../../common/model';
import { BinaryOperatorRuntime } from '../../common/runtimes';

export const and: BinaryOperatorRuntime = (leftSide, getRightSide) =>
  leftSide.flatMap(left => getRightSide()
    .map(right => left.entrySeq()
      .concat(right.entrySeq())
      .groupBy(([label]) => label)
      .map(group => group
        .map(([__, match]) => match)
        .reduce((acc: Match, match: Match) => acc.and(match)))
      .toMap()));
