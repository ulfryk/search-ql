import { Map } from 'immutable';

import { NumberExpression } from '../../ast';
import { NodeEvaluation } from '../../common/model';
import { ParserConfig } from '../../config';
import { Tester } from '../tester';

export class NumberExpressionTester extends Tester<number, NumberExpression, null> {

  constructor(
    public readonly ast: NumberExpression,
    public readonly config: ParserConfig,
  ) {
    super(ast, null, config);
  }

  public test(values: Map<string, string>) {
    return NodeEvaluation.ofNumber(values, this.ast)(this.ast.preparedValue);
  }

}
