import { List, Map } from 'immutable';

import { Expression, NodeEvaluation } from '../model';

export type FunctionRuntime<R> =
  (values: Map<string, string>, node: Expression) =>
    (matches: List<() => NodeEvaluation<any>/* FIXME: how to do it better ? */>) =>
      NodeEvaluation<R>;
