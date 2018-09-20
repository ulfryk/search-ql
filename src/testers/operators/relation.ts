import { RelationRuntime } from './relation-runtime';

import { NodeEvaluation, Ordering } from '../../common/model';
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
