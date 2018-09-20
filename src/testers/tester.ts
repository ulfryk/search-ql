import { Map } from 'immutable';

import { Expression } from '../index';

import { TesterConfig } from './config';
import { NodeEvaluation } from './model';

export abstract class Tester<R, A extends Expression, C> {

  public static fromAst(_config: TesterConfig): (ast: Expression) => Tester<any, Expression, any> {
    throw Error('unimplemented');
  }

  constructor(
    public readonly ast: A,
    public readonly children: C,
    public readonly config: TesterConfig,
  ) {}

  public abstract test(values: Map<string, string>): NodeEvaluation<R>;

}
