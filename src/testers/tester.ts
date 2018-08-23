import { Map } from 'immutable';
import { Maybe } from 'monet';

import { Expression } from '../expressions';
import { Match } from '../match';
import { SyntaxConfig } from '../syntax-config';

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
