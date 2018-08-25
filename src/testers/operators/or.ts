import { Maybe } from 'monet';

import { BinaryOperatorRuntime } from './binary-operator-runtime';

export const or: BinaryOperatorRuntime = (leftSide, getRightSide) =>
  leftSide.cata(getRightSide, left => Maybe.of(left));
