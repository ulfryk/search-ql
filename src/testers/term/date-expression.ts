import { Map } from 'immutable';
import { Maybe, None } from 'monet';

import { DateExpression } from '../../ast';
import { SyntaxConfig } from '../../config';
import { Match } from '../../match';
import { Tester } from '../tester';

export class DateExpressionTester extends Tester<DateExpression, null> {

  constructor(
    public readonly ast: DateExpression,
    public readonly config: SyntaxConfig,
  ) {
    super(ast, null, config);
  }

  public test(_values: Map<string, string>): Maybe<Map<string, Match>> {
    return None(); // TBD
  }

}
