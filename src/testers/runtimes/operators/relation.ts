import { Ordering } from '../../../index';

import { NodeEvaluation } from '../../model';
import { RelationRuntime } from './relation-runtime';
import { RelationValue } from './relation-value';

export const relational =
  <T>(getValue: (ord: Ordering) => boolean): RelationRuntime<T> =>
    (values, node) => (
      left: NodeEvaluation<T>,
      right: NodeEvaluation<T>,
    ) =>
      NodeEvaluation.ofBoolean(values, node)(getValue(
        RelationValue.fromEvaluation<T>(left, values)
        .compare(RelationValue.fromEvaluation<T>(right, values))));
