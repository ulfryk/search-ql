import { Map } from 'immutable';
import { Maybe } from 'monet';

import { Expression } from '../ast';
import { Match } from '../common/model';
import { SyntaxConfig } from '../config';

export abstract class Tester<A extends Expression, C> {

  public static fromAst(_config: SyntaxConfig): (ast: Expression) => Tester<Expression, any> {
    throw Error('unimplemented');
  }

  constructor(
    public readonly ast: A,
    public readonly children: C,
    public readonly config: SyntaxConfig,
  ) {}

  public abstract test(_values: Map<string, string>): Maybe<Map<string, Match>>;

}
