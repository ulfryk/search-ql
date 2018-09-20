import { List, Map } from 'immutable';

import { Expression } from '../../../index';

import { NodeEvaluation } from '../node-evaluation';

export type FunctionRuntime<R> =
  (values: Map<string, string>, node: Expression) =>
    (matches: List<() => NodeEvaluation<any>/* FIXME: how to do it better ? */>) =>
      NodeEvaluation<R>;
