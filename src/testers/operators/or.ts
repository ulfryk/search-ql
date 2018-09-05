import { Maybe } from 'monet';

import { NodeEvaluation } from '../../common/model';
import { BinaryOperatorRuntime } from '../../common/runtimes';

export const or: BinaryOperatorRuntime<boolean, boolean, boolean> =
  (values, node) => (left, right) =>
    NodeEvaluation.ofBoolean(values, node)(
      left.value || right.value,
      () => left.matches().cata(right.matches, __ => Maybe.of(__)));
