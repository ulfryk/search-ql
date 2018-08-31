import { Map } from 'immutable';
import { Maybe, Some } from 'monet';

import { DateExpression } from '../../ast';
import { Match } from '../../common/model';
import { SyntaxConfig } from '../../config';
import { Tester } from '../tester';

export class DateExpressionTester extends Tester<DateExpression, null> {

  constructor(
    public readonly ast: DateExpression,
    public readonly config: SyntaxConfig,
  ) {
    super(ast, null, config);
  }

  public test(values: Map<string, string>): Maybe<Map<string, Match>> {
    return Some(values
      .filter(value => DateExpression.prepareValue(value) === this.ast.preparedValue)
      .map(value => Match.whole(value))
      .toMap())
    .filter(filtered => !filtered.isEmpty());
  }

}
