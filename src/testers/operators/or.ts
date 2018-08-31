import { Maybe } from 'monet';

import { BinaryOperatorRuntime } from '../../common/runtimes';

export const or: BinaryOperatorRuntime = (leftSide, getRightSide) =>
  leftSide.cata(getRightSide, left => Maybe.of(left));
