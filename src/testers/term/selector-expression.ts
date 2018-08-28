import { Map } from 'immutable';
import { Maybe, Some } from 'monet';

import { SelectorExpression } from '../../ast';
import { SyntaxConfig } from '../../config';
import { Match } from '../../match';
import { Tester } from '../tester';

export class SelectorExpressionTester extends Tester<SelectorExpression, null> {

  constructor(
    public readonly ast: SelectorExpression,
    public readonly config: SyntaxConfig,
  ) {
    super(ast, null, config);
  }

  public test(values: Map<string, string>): Maybe<Map<string, Match>> {
    return Some(values
        .filter((_value, key) => key === this.ast.value)
        .map(value => Match.whole(value))
        .toMap())
      .filter(filtered => !filtered.isEmpty());
  }

}
