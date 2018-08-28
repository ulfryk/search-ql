import { Map } from 'immutable';
import { Maybe, None } from 'monet';

import { NumberExpression } from '../../ast';
import { SyntaxConfig } from '../../config';
import { Match } from '../../match';
import { Tester } from '../tester';

export class NumberExpressionTester extends Tester<NumberExpression, null> {

  constructor(
    public readonly ast: NumberExpression,
    public readonly config: SyntaxConfig,
  ) {
    super(ast, null, config);
  }

  public test(_values: Map<string, string>): Maybe<Map<string, Match>> {
    return None(); // TBD
  }

}
