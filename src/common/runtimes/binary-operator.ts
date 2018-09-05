import { Map } from 'immutable';

import { Expression, NodeEvaluation } from '../model';

// IDefaultInput below is not used right now. Yet will be in future :)
export type BinaryOperatorRuntime<L, R, V> =
  (values: Map<string, string>, node: Expression) => (
    a: NodeEvaluation<L>,
    b: NodeEvaluation<R>,
  ) => NodeEvaluation<V>;
