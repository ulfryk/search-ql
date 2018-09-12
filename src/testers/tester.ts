import { Map } from 'immutable';

import { Expression, NodeEvaluation } from '../common/model';
import { ParserConfig } from '../config';

export abstract class Tester<R, A extends Expression, C> {

  public static fromAst(_config: ParserConfig): (ast: Expression) => Tester<any, Expression, any> {
    throw Error('unimplemented');
  }

  constructor(
    public readonly ast: A,
    public readonly children: C,
    public readonly config: ParserConfig,
  ) {}

  public abstract test(values: Map<string, string>): NodeEvaluation<R>;

}
