import { Map } from 'immutable';

import { Expression, NodeEvaluation } from '../common/model';
import { SyntaxConfig } from '../config';

export abstract class Tester<R, A extends Expression, C> {

  public static fromAst(_config: SyntaxConfig): (ast: Expression) => Tester<any, Expression, any> {
    throw Error('unimplemented');
  }

  constructor(
    public readonly ast: A,
    public readonly children: C,
    public readonly config: SyntaxConfig,
  ) {}

  public abstract test(values: Map<string, string>): NodeEvaluation<R>;

}
