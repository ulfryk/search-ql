import { Map } from 'immutable';
import { Maybe, Some } from 'monet';

import { NumberExpression } from '../../ast';
import { Match } from '../../common/model';
import { SyntaxConfig } from '../../config';
import { Tester } from '../tester';

export class NumberExpressionTester extends Tester<NumberExpression, null> {

  constructor(
    public readonly ast: NumberExpression,
    public readonly config: SyntaxConfig,
  ) {
    super(ast, null, config);
  }

  public test(values: Map<string, string>): Maybe<Map<string, Match>> {
    return Some(values
      .filter(value => NumberExpression.prepareValue(value) === this.ast.preparedValue)
      .map(value => Match.whole(value))
      .toMap())
    .filter(filtered => !filtered.isEmpty());
  }

}
