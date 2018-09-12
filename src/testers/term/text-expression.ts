import { Map } from 'immutable';

import { TextExpression } from '../../ast';
import { NodeEvaluation } from '../../common/model';
import { ParserConfig } from '../../config';
import { Tester } from '../tester';

export class TextExpressionTester extends Tester<string, TextExpression, null> {

  constructor(
    public readonly ast: TextExpression,
    public readonly config: ParserConfig,
  ) {
    super(ast, null, config);
  }

  public test(values: Map<string, string>) {
    return NodeEvaluation.ofText(values, this.ast)(this.ast.value);
  }

}
