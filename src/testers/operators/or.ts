import { Maybe } from 'monet';

import { NodeEvaluation } from '../../common/model';
import { BinaryOperatorRuntime } from '../../common/runtimes';

export const or: BinaryOperatorRuntime<boolean, boolean, boolean> =
  (values, node) => (left, right) => {
    const value = left.value || right.value;
    const matches = () => left.matches().cata(right.matches, __ => Maybe.of(__));

    return NodeEvaluation.fromLogic(left.type, right.type)(values, node)(value, matches);
  };
